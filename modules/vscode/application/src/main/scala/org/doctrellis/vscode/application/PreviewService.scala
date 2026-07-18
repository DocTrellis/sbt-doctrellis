package org.doctrellis.vscode.application

import cats.effect.std.{Mutex, Supervisor}
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import org.doctrellis.vscode.domain.*

final case class RunningProcess(awaitExit: IO[ProcessExit])

final case class PreviewPorts(
    load: Workspace => IO[PreviewCommand],
    policy: Workspace => IO[PreviewPolicy],
    launch: (Workspace, PreviewCommand) => Resource[IO, RunningProcess],
    probe: PreviewUrl => IO[Boolean],
    open: PreviewUrl => IO[Unit],
    info: String => IO[Unit],
    error: String => IO[Unit],
)

sealed abstract class PreviewFailure(message: String)
    extends RuntimeException(message)

final case class PreviewTimedOut(url: PreviewUrl, timeout: StartupTimeout)
    extends PreviewFailure(
      s"Timed out after ${timeout.duration.toMillis} ms waiting for ${url.value}.",
    )

final case class PreviewExited(url: PreviewUrl, exit: ProcessExit)
    extends PreviewFailure(
      s"Preview process exited before ${url.value} became ready (${exit.render}).",
    )

final class PreviewService private (
    ports: PreviewPorts,
    sessions: Ref[IO, Map[String, PreviewSession]],
    mutex: Mutex[IO],
    supervisor: Supervisor[IO],
):
  def start(workspace: Workspace): IO[Unit] =
    mutex.lock.surround(getOrStart(workspace)).flatMap(_.awaitReady)

  def stop(workspace: Workspace): IO[Unit] =
    stopSession(workspace, reportMissing = true)

  def stopSilently(workspace: Workspace): IO[Unit] =
    stopSession(workspace, reportMissing = false)

  private def stopSession(
      workspace: Workspace,
      reportMissing: Boolean,
  ): IO[Unit] =
    mutex.lock.surround:
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(session) =>
            session.status.set(PreviewStatus.Stopping) *>
              session.ready.complete(Right(())).void *>
              session.close.guarantee(removeExact(workspace.key, session))
          case None if reportMissing =>
            ports.info(
              s"No DocTrellis preview is running for ${workspace.name}.",
            )
          case None => IO.unit

  def restart(workspace: Workspace): IO[Unit] =
    stop(workspace) *> start(workspace)

  def open(workspace: Workspace): IO[Unit] =
    sessions.get.flatMap:
      _.get(workspace.key) match
        case Some(session) => ports.open(session.command.url)
        case None          =>
          ports.load(workspace).flatMap(command => ports.open(command.url))

  def stopAll: IO[Unit] =
    mutex.lock.surround:
      sessions
        .getAndSet(Map.empty)
        .flatMap:
          _.values.toList.traverse_(session =>
            (session.status.set(PreviewStatus.Stopping) *>
              session.ready.complete(Right(())).void *>
              session.close).handleErrorWith(error =>
              report(
                s"Failed to stop preview at ${session.command.url.value}: ${message(error)}",
              ),
            ),
          )

  private def getOrStart(workspace: Workspace): IO[PreviewSession] =
    sessions.get.flatMap:
      _.get(workspace.key) match
        case Some(session) =>
          ports
            .info(
              s"DocTrellis preview is already active for ${workspace.name}.",
            )
            .as(session)
        case None => createSession(workspace)

  private def createSession(workspace: Workspace): IO[PreviewSession] =
    for
      command <- ports.load(workspace)
      policy  <- ports.policy(workspace)
      session <- installSession(workspace, command, policy)
    yield session

  private def installSession(
      workspace: Workspace,
      command: PreviewCommand,
      policy: PreviewPolicy,
  ): IO[PreviewSession] =
    IO.uncancelable: _ =>
      allocateSession(workspace, command, policy).flatMap: session =>
        val run = monitor(workspace, session)
          .handleErrorWith: error =>
            session.ready.complete(Left(error)).void *>
              report(
                s"Preview lifecycle failed for ${workspace.name}: ${message(error)}",
              )
          .guarantee(
            removeExact(workspace.key, session) *>
              session.close.handleErrorWith(error =>
                report(
                  s"Failed to release preview for ${workspace.name}: ${message(error)}",
                ),
              ),
          )
        val publish = sessions.update(_.updated(workspace.key, session)) *>
          supervisor.supervise(run).void
        publish
          .onError(_ =>
            session.close.attempt *> removeExact(workspace.key, session),
          )
          .as(session)

  private def allocateSession(
      workspace: Workspace,
      command: PreviewCommand,
      policy: PreviewPolicy,
  ): IO[PreviewSession] =
    ports
      .launch(workspace, command)
      .allocated
      .flatMap:
        case (running, release) =>
          for
            status <- Ref.of[IO, PreviewStatus](PreviewStatus.Starting)
            ready  <- Deferred[IO, Either[Throwable, Unit]]
            closed <- Ref.of[IO, Boolean](false)
            close = closed
              .modify:
                case true  => true -> IO.unit
                case false => true -> release
              .flatten
          yield PreviewSession(command, policy, running, status, ready, close)

  private def monitor(workspace: Workspace, session: PreviewSession): IO[Unit] =
    val startup = awaitReady(session.command, session.policy).attempt
      .race(session.running.awaitExit.attempt)
    startup
      .flatMap:
        case Left(Right(_))     => onReady(workspace, session)
        case Left(Left(error))  => session.ready.complete(Left(error)).void
        case Right(Right(exit)) =>
          session.ready
            .complete(Left(PreviewExited(session.command.url, exit)))
            .void
        case Right(Left(error)) => session.ready.complete(Left(error)).void

  private def onReady(workspace: Workspace, session: PreviewSession): IO[Unit] =
    val open =
      if session.policy.openWhenReady then
        ports
          .open(session.command.url)
          .handleErrorWith(error => report(message(error)))
      else IO.unit

    session.status.set(PreviewStatus.Ready) *>
      session.ready.complete(Right(())).void *>
      ports
        .info(s"DocTrellis preview is ready for ${workspace.name}.")
        .attempt
        .void *>
      open *>
      session.running.awaitExit.attempt.flatMap:
        case Left(error) =>
          report(
            s"DocTrellis preview for ${workspace.name} failed: ${message(error)}",
          )
        case Right(exit) =>
          session.status.get.flatMap:
            case PreviewStatus.Stopping => IO.unit
            case _                      =>
              report(
                s"DocTrellis preview for ${workspace.name} exited (${exit.render}).",
              )

  private def awaitReady(
      command: PreviewCommand,
      policy: PreviewPolicy,
  ): IO[Unit] =
    def loop: IO[Unit] =
      ports
        .probe(command.url)
        .flatMap:
          case true  => IO.unit
          case false => IO.sleep(policy.pollInterval.duration) *> loop

    loop.timeoutTo(
      command.startupTimeout.duration,
      IO.raiseError(PreviewTimedOut(command.url, command.startupTimeout)),
    )

  private def removeExact(key: String, expected: PreviewSession): IO[Unit] =
    sessions.update: current =>
      current.get(key) match
        case Some(actual) if actual.eq(expected) => current.removed(key)
        case _                                   => current

  private def report(message: String): IO[Unit] =
    ports.error(message).handleError(_ => ())

  private def message(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString)

private final case class PreviewSession(
    command: PreviewCommand,
    policy: PreviewPolicy,
    running: RunningProcess,
    status: Ref[IO, PreviewStatus],
    ready: Deferred[IO, Either[Throwable, Unit]],
    close: IO[Unit],
):
  def awaitReady: IO[Unit] = ready.get.flatMap(_.liftTo[IO])

object PreviewService:
  def resource(ports: PreviewPorts): Resource[IO, PreviewService] =
    Supervisor[IO](await = false).flatMap: supervisor =>
      for
        sessions <- Resource.eval(
          Ref.of[IO, Map[String, PreviewSession]](Map.empty),
        )
        mutex <- Resource.eval(Mutex[IO])
        service = PreviewService(ports, sessions, mutex, supervisor)
        _ <- Resource.make(IO.unit)(_ => service.stopAll)
      yield service
