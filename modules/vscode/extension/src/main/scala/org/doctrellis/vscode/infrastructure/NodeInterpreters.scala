package org.doctrellis.vscode.infrastructure

import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import cats.effect.{Deferred, IO, Resource}
import cats.effect.std.Dispatcher
import cats.syntax.all.*

import org.doctrellis.vscode.application.RunningProcess
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

private[infrastructure] enum ProcessPlatform:
  case Posix, Windows

private[infrastructure] object ProcessPlatform:
  def parse(value: String): ProcessPlatform =
    if value == "win32" then ProcessPlatform.Windows else ProcessPlatform.Posix

private[infrastructure] object NodeError:
  def hasCode(error: Throwable, expected: String): Boolean =
    val raw = error match
      case js.JavaScriptException(exception) => exception
      case _                                 => error.asInstanceOf[js.Any]
    val code = raw.asInstanceOf[js.Dynamic].selectDynamic("code")
    NodeProcess.present(code) && code.toString == expected

private[infrastructure] final case class ProcessShutdownTimeouts(
    graceful: FiniteDuration,
    forced: FiniteDuration,
    pollInterval: FiniteDuration,
)

private[infrastructure] object ProcessShutdownTimeouts:
  val default: ProcessShutdownTimeouts = ProcessShutdownTimeouts(
    graceful = 3.seconds,
    forced = 1.second,
    pollInterval = 50.millis,
  )

private[infrastructure] object NodeProcess:
  def launch(
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
  )(
      workspace: Workspace,
      command: PreviewCommand,
  ): Resource[IO, RunningProcess[IO]] =
    launchWithTimeouts(
      dispatcher = dispatcher,
      append = append,
      timeouts = ProcessShutdownTimeouts.default,
      environment = Map.empty,
    )(workspace, command)

  def launchWithEnvironment(
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
      environment: Map[String, String],
  )(
      workspace: Workspace,
      command: PreviewCommand,
  ): Resource[IO, RunningProcess[IO]] =
    launchWithTimeouts(
      dispatcher = dispatcher,
      append = append,
      timeouts = ProcessShutdownTimeouts.default,
      environment = environment,
    )(workspace, command)

  private[infrastructure] def launchWithTimeouts(
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
      timeouts: ProcessShutdownTimeouts,
  )(
      workspace: Workspace,
      command: PreviewCommand,
  ): Resource[IO, RunningProcess[IO]] =
    launchWithTimeouts(
      dispatcher = dispatcher,
      append = append,
      timeouts = timeouts,
      environment = Map.empty,
    )(workspace, command)

  private def launchWithTimeouts(
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
      timeouts: ProcessShutdownTimeouts,
      environment: Map[String, String],
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
        .make(
          spawn(command, dispatcher, append, workspace.name, environment),
        )(emergencyStop)
        .flatMap: process =>
          Resource
            .make(
              attach(process, dispatcher, append, workspace.name, timeouts),
            )(
              _.shutdown,
            )
            .flatMap: handle =>
              Resource
                .eval(
                  append(
                    s"[${workspace.name}] Owned process tree PID: ${process.pid}\n",
                  ),
                )
                .as(handle)
        .map(handle => RunningProcess(handle.closed.get))

  private def spawn(
      command: PreviewCommand,
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
      workspaceName: String,
      environment: Map[String, String],
  ): IO[OwnedProcess] =
    IO.delay:
      val platform = ProcessPlatform.parse(NodeRuntime.platform)
      val options  = js.Dynamic.literal(
        cwd = command.workingDirectory.value,
        detached = platform == ProcessPlatform.Posix,
        shell = false,
        windowsHide = true,
      )
      if environment.nonEmpty then
        val variables = js.Dictionary.empty[String]
        NodeRuntime.env.foreach: (name, value) =>
          value.toOption.foreach(actual => variables.update(name, actual))
        environment.foreach: (name, value) =>
          variables.update(name, value)
        options.updateDynamic("env")(variables)
      val child = NodeChildProcess.spawn(
        command.executable.value,
        command.arguments.map(_.value).toJSArray,
        options,
      )
      child.pid.toOption.filter(_ > 0) match
        case Some(pid) =>
          OwnedProcess(child = child, pid = pid, platform = platform)
        case None =>
          val reportError: js.Function1[js.Any, Unit] = raw =>
            dispatcher.unsafeRunAndForget(
              append(
                s"[$workspaceName] Preview process spawn failed: ${errorMessage(raw)}\n",
              ).handleError(_ => ()),
            )
          child.once("error", reportError): Unit
          throw IllegalStateException(
            s"${command.executable.value} did not start with an owned process ID.",
          )

  private def attach(
      process: OwnedProcess,
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
      workspaceName: String,
      timeouts: ProcessShutdownTimeouts,
  ): IO[ProcessHandle] =
    Deferred[IO, ProcessExit].flatMap: closed =>
      IO.delay:
        var processError = Option.empty[String]
        val child        = process.child

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

        child.stdout.on("data", stdout): Unit
        child.stderr.on("data", stderr): Unit
        child.once("error", error): Unit
        child.once("close", close): Unit
        ProcessHandle(
          process = process,
          closed = closed,
          stdoutListener = stdout,
          stderrListener = stderr,
          errorListener = error,
          closeListener = close,
          append = append,
          workspaceName = workspaceName,
          timeouts = timeouts,
        )

  private def emergencyStop(process: OwnedProcess): IO[Unit] =
    process.isAlive
      .ifM(process.force, IO.unit)
      .handleError(_ => ())

  private[infrastructure] def terminal(child: ChildProcess): Boolean =
    present(child.exitCode) || present(child.signalCode)

  private[infrastructure] def present(value: js.Any): Boolean =
    value != null && !js.isUndefined(value)

  private[infrastructure] def number(value: js.Any): Option[Int] =
    Option.when(present(value))(value.asInstanceOf[Double].toInt)

  private def string(value: js.Any): Option[String] =
    Option.when(present(value))(value.toString)

  private def chunk(value: js.Any): String =
    value.asInstanceOf[js.Dynamic].toString().asInstanceOf[String]

  private[infrastructure] def errorMessage(value: js.Any): String =
    val message = value.asInstanceOf[js.Dynamic].message
    if present(message) then message.toString else value.toString

private[infrastructure] final case class OwnedProcess(
    child: ChildProcess,
    pid: Int,
    platform: ProcessPlatform,
):
  def graceful: IO[Unit] = platform match
    case ProcessPlatform.Posix   => signalGroup("SIGTERM")
    case ProcessPlatform.Windows => taskkill(force = false)

  def force: IO[Unit] = platform match
    case ProcessPlatform.Posix   => signalGroup("SIGKILL")
    case ProcessPlatform.Windows => taskkill(force = true)

  def isAlive: IO[Boolean] = platform match
    case ProcessPlatform.Posix   => groupIsAlive
    case ProcessPlatform.Windows => processIsAlive(pid)

  private def signalGroup(signal: String): IO[Unit] =
    IO.delay(NodeRuntime.kill(-pid, signal))
      .void
      .handleErrorWith: error =>
        if missingProcess(error) then IO.unit else IO.raiseError(error)

  private def groupIsAlive: IO[Boolean] =
    IO.delay(NodeRuntime.kill(-pid, 0))
      .as(true)
      .handleErrorWith: error =>
        if missingProcess(error) then IO.pure(false) else IO.raiseError(error)

  private def processIsAlive(processId: Int): IO[Boolean] =
    IO.delay(NodeRuntime.kill(processId, 0))
      .as(true)
      .handleErrorWith: error =>
        if missingProcess(error) then IO.pure(false) else IO.raiseError(error)

  private def taskkill(force: Boolean): IO[Unit] =
    runTaskkill(force)

  private def runTaskkill(force: Boolean): IO[Unit] =
    val arguments = OwnedProcess.taskkillArguments(pid, force)
    IO.async[Unit]: complete =>
      IO.delay:
        var completed = false

        def finish(result: Either[Throwable, Unit]): Unit =
          if !completed then
            completed = true
            complete(result)

        val task = NodeChildProcess.spawn(
          OwnedProcess.configuredTaskkillExecutable,
          arguments.toJSArray,
          js.Dynamic.literal(
            shell = false,
            stdio = "ignore",
            windowsHide = true,
          ),
        )
        val error: js.Function1[js.Any, Unit] = raw =>
          finish(
            Left(
              IllegalStateException(NodeProcess.errorMessage(raw)),
            ),
          )
        val close: js.Function2[js.Any, js.Any, Unit] = (code, _) =>
          NodeProcess.number(code) match
            case Some(0) => finish(Right(()))
            case status  =>
              finish(
                Left(
                  IllegalStateException(
                    s"taskkill exited with status ${status.getOrElse(-1)}",
                  ),
                ),
              )
        task.once("error", error): Unit
        task.once("close", close): Unit
        Some(
          IO.delay:
            val cancellationError: js.Function1[js.Any, Unit] = _ => ()
            task.once("error", cancellationError): Unit
            task.off("error", error): Unit
            task.off("close", close): Unit
            if !NodeProcess.terminal(task) then task.kill("SIGKILL"): Unit
          .void
            .handleError(_ => ()),
        )

  private def missingProcess(error: Throwable): Boolean =
    NodeError.hasCode(error, "ESRCH")

private[infrastructure] object OwnedProcess:
  def permissionDenied(error: Throwable): Boolean =
    NodeError.hasCode(error, "EPERM")

  def configuredTaskkillExecutable: String =
    taskkillExecutable(
      NodeRuntime.env
        .get("SystemRoot")
        .flatMap(_.toOption),
    )

  def taskkillExecutable(systemRoot: Option[String]): String =
    systemRoot
      .filter(_.nonEmpty)
      .map(root => s"$root\\System32\\taskkill.exe")
      .getOrElse("taskkill.exe")

  def taskkillArguments(pid: Int, force: Boolean): Vector[String] =
    Vector("/PID", pid.toString, "/T") ++ Option.when(force)("/F")

private[infrastructure] object ProcessShutdown:
  def bounded(
      label: String,
      timeout: FiniteDuration,
      action: IO[Unit],
  ): IO[Unit] =
    action
      .adaptError: error =>
        IllegalStateException(s"$label failed: ${error.getMessage}", error)
      .timeoutTo(
        timeout,
        IO.raiseError(
          IllegalStateException(
            s"$label timed out after ${timeout.toMillis} ms.",
          ),
        ),
      )

  def gracefulThenForce(
      graceful: IO[Unit],
      awaitGraceful: IO[Unit],
      force: IO[Unit],
      awaitForced: IO[Unit],
      onForce: Throwable => IO[Unit],
  ): IO[Unit] =
    (graceful *> awaitGraceful).handleErrorWith: error =>
      onForce(error) *> force *> awaitForced

private[infrastructure] object ProcessClose:
  def confirm(
      closed: Deferred[IO, ProcessExit],
      timeout: FiniteDuration,
      synthesize: Boolean,
      append: String => IO[Unit],
      workspaceName: String,
      pid: Int,
  ): IO[Unit] =
    val missing = IllegalStateException(
      s"Preview process $pid exited without a close notification.",
    )
    val fallback =
      if synthesize then
        append(
          s"[$workspaceName] Owned process group $pid exited without a close event; treating the confirmed group exit as terminal.\n",
        ).handleError(_ => ()) *>
          closed
            .complete(
              ProcessExit(
                code = None,
                signal = None,
                error = Some("close notification was not delivered"),
              ),
            )
            .void
      else IO.raiseError(missing)
    closed.get.void.timeoutTo(timeout, fallback)

private final case class ProcessHandle(
    process: OwnedProcess,
    closed: Deferred[IO, ProcessExit],
    stdoutListener: js.Function,
    stderrListener: js.Function,
    errorListener: js.Function,
    closeListener: js.Function,
    append: String => IO[Unit],
    workspaceName: String,
    timeouts: ProcessShutdownTimeouts,
):
  def shutdown: IO[Unit] =
    stop.attempt.flatMap:
      case Right(_)    => detach
      case Left(error) => IO.raiseError(error)

  private def stop: IO[Unit] =
    contextual("initial ownership probe", process.isAlive).flatMap:
      case false => awaitClose(timeouts.forced)
      case true  =>
        ProcessShutdown.gracefulThenForce(
          graceful = ProcessShutdown.bounded(
            label = "graceful signal",
            timeout = timeouts.forced,
            action = process.graceful,
          ),
          awaitGraceful = awaitTermination(timeouts.graceful),
          force = ProcessShutdown.bounded(
            label = "forced signal",
            timeout = timeouts.forced,
            action = process.force,
          ),
          awaitForced = awaitTermination(timeouts.forced),
          onForce = error =>
            append(
              s"[$workspaceName] Graceful preview shutdown failed (${error.getMessage}); forcing its owned process tree.\n",
            ).handleError(_ => ()),
        )

  private def contextual[A](label: String, action: IO[A]): IO[A] =
    action.adaptError: error =>
      IllegalStateException(s"$label failed: ${error.getMessage}", error)

  private def awaitTermination(timeout: FiniteDuration): IO[Unit] =
    awaitTreeExit
      .timeoutTo(
        timeout,
        IO.raiseError(
          IllegalStateException(
            s"Preview process tree ${process.pid} did not exit within ${timeout.toMillis} ms.",
          ),
        ),
      ) *> confirmClose

  private def awaitTreeExit: IO[Unit] =
    process.isAlive.attempt.flatMap:
      case Right(false) => IO.unit
      case Right(true)  => IO.sleep(timeouts.pollInterval) *> awaitTreeExit
      case Left(error) if OwnedProcess.permissionDenied(error) =>
        IO.sleep(timeouts.pollInterval) *> awaitTreeExit
      case Left(error) => IO.raiseError(error)

  private def awaitClose(timeout: FiniteDuration): IO[Unit] =
    confirmClose(timeout)

  private def confirmClose: IO[Unit] =
    confirmClose(timeouts.forced)

  private def confirmClose(timeout: FiniteDuration): IO[Unit] =
    ProcessClose.confirm(
      closed = closed,
      timeout = timeout,
      synthesize = process.platform == ProcessPlatform.Posix,
      append = append,
      workspaceName = workspaceName,
      pid = process.pid,
    )

  private def detach: IO[Unit] =
    IO.delay:
      val child = process.child
      child.stdout.off("data", stdoutListener): Unit
      child.stderr.off("data", stderrListener): Unit
      child.off("error", errorListener): Unit
      child.off("close", closeListener): Unit

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
        active.once("error", errorListener): Unit

        Some:
          IO
            .delay:
              val cancellationErrorListener: js.Function1[js.Any, Unit] =
                _ => ()
              completed = true
              active.once("error", cancellationErrorListener): Unit
              active.off("response", responseListener): Unit
              active.destroy()
            .handleError(_ => ())
