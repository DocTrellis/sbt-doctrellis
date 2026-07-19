package org.doctrellis.vscode.infrastructure

import cats.effect.std.Dispatcher
import cats.effect.{Deferred, IO, Resource}
import cats.syntax.all.*
import org.doctrellis.vscode.application.RunningProcess
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

private[infrastructure] object NodeProcess:
  def launch(
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
  )(
      workspace: Workspace,
      command: PreviewCommand,
  ): Resource[IO, RunningProcess[IO]] =
    val announce = append(
      s"\n[${workspace.name}] Starting ${command.executable.value} ${command.arguments.map(_.value).mkString(" ")}\n" +
        s"[${workspace.name}] Working directory: ${command.workingDirectory.value}\n",
    )
    Resource.eval(announce) *>
      Resource
        .make(spawn(command))(emergencyStop)
        .flatMap: child =>
          Resource.make(attach(child, dispatcher, append, workspace.name))(
            _.shutdown,
          )
        .map(handle => RunningProcess(handle.closed.get))

  private def spawn(command: PreviewCommand): IO[ChildProcess] =
    IO.delay:
      NodeChildProcess.spawn(
        command.executable.value,
        command.arguments.map(_.value).toJSArray,
        js.Dynamic.literal(
          cwd = command.workingDirectory.value,
          shell = false,
          windowsHide = true,
        ),
      )

  private def attach(
      child: ChildProcess,
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
      workspaceName: String,
  ): IO[ProcessHandle] =
    Deferred[IO, ProcessExit].flatMap: closed =>
      IO.delay:
        var processError = Option.empty[String]

        val stdout: js.Function1[js.Any, Unit] = data =>
          dispatcher.unsafeRunAndForget(
            append(chunk(data)).handleError(_ => ()),
          )
        val stderr: js.Function1[js.Any, Unit] = data =>
          dispatcher.unsafeRunAndForget(
            append(chunk(data)).handleError(_ => ()),
          )
        val error: js.Function1[js.Any, Unit] =
          raw => processError = Some(errorMessage(raw))
        val close: js.Function2[js.Any, js.Any, Unit] = (code, signal) =>
          val exit = ProcessExit(number(code), string(signal), processError)
          dispatcher.unsafeRunAndForget(closed.complete(exit).void)

        child.stdout.on("data", stdout)
        child.stderr.on("data", stderr)
        child.once("error", error)
        child.once("close", close)
        ProcessHandle(
          child,
          closed,
          stdout,
          stderr,
          error,
          close,
          append,
          workspaceName,
        )

  private def emergencyStop(child: ChildProcess): IO[Unit] =
    IO
      .delay:
        if !terminal(child) then child.kill("SIGKILL")
      .void
      .handleError(_ => ())

  private def terminal(child: ChildProcess): Boolean =
    present(child.exitCode) || present(child.signalCode)

  private def present(value: js.Any): Boolean =
    value != null && !js.isUndefined(value)

  private def number(value: js.Any): Option[Int] =
    Option.when(present(value))(value.asInstanceOf[Double].toInt)

  private def string(value: js.Any): Option[String] =
    Option.when(present(value))(value.toString)

  private def chunk(value: js.Any): String =
    value.asInstanceOf[js.Dynamic].toString().asInstanceOf[String]

  private def errorMessage(value: js.Any): String =
    val message = value.asInstanceOf[js.Dynamic].message
    if present(message) then message.toString else value.toString

private final case class ProcessHandle(
    child: ChildProcess,
    closed: Deferred[IO, ProcessExit],
    stdoutListener: js.Function,
    stderrListener: js.Function,
    errorListener: js.Function,
    closeListener: js.Function,
    append: String => IO[Unit],
    workspaceName: String,
):
  def shutdown: IO[Unit] =
    val timedOutExit = ProcessExit(
      None,
      None,
      Some("shutdown confirmation timed out"),
    )
    val stop = closed.tryGet.flatMap:
      case Some(_) => IO.unit
      case None    =>
        signal("SIGTERM") *>
          closed.get
            .timeoutTo(
              3.seconds,
              signal("SIGKILL") *>
                closed.get.timeoutTo(
                  1.second,
                  closed.complete(timedOutExit).void *>
                    append(
                      s"[$workspaceName] Preview process did not confirm shutdown.\n",
                    ).handleError(_ => ()),
                ),
            )
            .void
    stop.guarantee(detach)

  private def signal(name: String): IO[Unit] =
    IO.delay(child.kill(name)).void.handleError(_ => ())

  private def detach: IO[Unit] =
    IO.delay:
      child.stdout.off("data", stdoutListener)
      child.stderr.off("data", stderrListener)
      child.off("error", errorListener)
      child.off("close", closeListener)

private[infrastructure] object NodeHttpProbe:
  def ready(url: PreviewUrl): IO[Boolean] =
    request(url)
      .timeoutTo(2.seconds, IO.pure(false))
      .handleError(_ => false)

  private def request(url: PreviewUrl): IO[Boolean] =
    IO.async[Boolean]: complete =>
      IO.delay:
        var completed = false

        def finish(value: Boolean): Unit =
          if !completed then
            completed = true
            complete(Right(value))

        val responseListener: js.Function1[IncomingMessage, Unit] = response =>
          response.resume()
          val status = response.statusCode.toOption.getOrElse(0)
          finish(status >= 200 && status < 400)
        val errorListener: js.Function1[js.Any, Unit] = _ => finish(false)

        val active =
          if url.value.startsWith("https://") then
            NodeHttps.get(url.value, responseListener)
          else NodeHttp.get(url.value, responseListener)
        active.once("error", errorListener)

        Some:
          IO
            .delay:
              val cancellationErrorListener: js.Function1[js.Any, Unit] =
                _ => ()
              completed = true
              active.once("error", cancellationErrorListener)
              active.off("response", responseListener)
              active.destroy()
            .handleError(_ => ())
