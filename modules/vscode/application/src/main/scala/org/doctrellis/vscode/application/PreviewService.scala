package org.doctrellis.vscode.application

import cats.effect.kernel.Temporal
import cats.effect.std.{Mutex, Supervisor}
import cats.effect.{Deferred, Ref, Resource}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import org.doctrellis.vscode.domain.*

final case class RunningProcess[F[_]](awaitExit: F[ProcessExit])

final case class PreviewPorts[F[_]](
    load: Workspace => F[PreviewCommand],
    policy: Workspace => F[PreviewPolicy],
    launch: (Workspace, PreviewCommand) => Resource[F, RunningProcess[F]],
    probe: PreviewUrl => F[Boolean],
    show: (Workspace, PreviewViewState) => F[Unit],
    update: (Workspace, PreviewViewState) => F[Unit],
    openExternal: PreviewUrl => F[Unit],
    info: String => F[Unit],
    error: String => F[Unit],
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

final class PreviewService[F[_]] private (
    ports: PreviewPorts[F],
    sessions: Ref[F, Map[String, PreviewSession[F]]],
    mutex: Mutex[F],
    supervisor: Supervisor[F],
)(using F: Temporal[F]):
  def start(workspace: Workspace): F[Unit] =
    mutex.lock.surround(getOrStart(workspace)).flatMap(_.awaitReady)

  def stop(workspace: Workspace): F[Unit] =
    stopSession(workspace, reportMissing = true)

  def stopSilently(workspace: Workspace): F[Unit] =
    stopSession(workspace, reportMissing = false)

  private def stopSession(
      workspace: Workspace,
      reportMissing: Boolean,
  ): F[Unit] =
    signalStopping(workspace.key).flatMap: wasActive =>
      mutex.lock.surround:
        sessions.get.flatMap:
          _.get(workspace.key) match
            case Some(session) =>
              session.status.set(PreviewStatus.Stopping) *>
                session.ready.complete(Right(())).void *>
                session.close.guarantee(
                  removeExact(workspace.key, session) *>
                    updateView(workspace, PreviewViewState.Stopped),
                )
            case None if reportMissing && !wasActive =>
              ports.info(
                s"No DocTrellis preview is running for ${workspace.name}.",
              )
            case None if wasActive =>
              updateView(workspace, PreviewViewState.Stopped)
            case None => F.unit

  def restart(workspace: Workspace): F[Unit] =
    stop(workspace) *> start(workspace)

  def open(workspace: Workspace): F[Unit] =
    sessions.get.flatMap:
      _.get(workspace.key) match
        case Some(session) =>
          session.status.get.flatMap: initial =>
            showView(workspace, sessionView(session, initial)) *>
              convergeOpen(workspace, session, initial)
        case None =>
          ports
            .load(workspace)
            .flatMap: command =>
              ports
                .probe(command.url)
                .flatMap: ready =>
                  val state =
                    if ready then PreviewViewState.Ready(command.url)
                    else PreviewViewState.Stopped
                  showView(workspace, state)

  def openExternal(workspace: Workspace): F[Unit] =
    sessions.get.flatMap:
      _.get(workspace.key) match
        case Some(session) => ports.openExternal(session.command.url)
        case None          =>
          ports
            .load(workspace)
            .flatMap(command => ports.openExternal(command.url))

  def stopAll: F[Unit] =
    mutex.lock.surround:
      sessions
        .getAndSet(Map.empty)
        .flatMap:
          _.values.toList.traverse_(session =>
            (session.status.set(PreviewStatus.Stopping) *>
              session.ready.complete(Right(())).void *>
              session.close *>
              updateView(session.workspace, PreviewViewState.Stopped))
              .handleErrorWith(error =>
                report(
                  s"Failed to stop preview at ${session.command.url.value}: ${message(error)}",
                ),
              ),
          )

  private def getOrStart(workspace: Workspace): F[PreviewSession[F]] =
    sessions.get.flatMap:
      _.get(workspace.key) match
        case Some(session) =>
          ports
            .info(
              s"DocTrellis preview is already active for ${workspace.name}.",
            )
            .as(session)
        case None => createSession(workspace)

  private def createSession(workspace: Workspace): F[PreviewSession[F]] =
    (for
      command <- ports.load(workspace)
      policy  <- ports.policy(workspace)
      session <- installSession(workspace, command, policy)
    yield session).handleErrorWith: error =>
      updateView(
        workspace,
        PreviewViewState.Failed(message(error)),
      ) *> F.raiseError(error)

  private def installSession(
      workspace: Workspace,
      command: PreviewCommand,
      policy: PreviewPolicy,
  ): F[PreviewSession[F]] =
    F.uncancelable: _ =>
      allocateSession(workspace, command, policy).flatMap: session =>
        val run = monitor(workspace, session)
          .handleErrorWith: error =>
            session.ready.complete(Left(error)).void *>
              updateView(
                workspace,
                PreviewViewState.Failed(message(error)),
              ) *>
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
          updateView(workspace, PreviewViewState.Starting) *>
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
  ): F[PreviewSession[F]] =
    ports
      .launch(workspace, command)
      .allocated
      .flatMap:
        case (running, release) =>
          for
            status <- Ref.of[F, PreviewStatus](PreviewStatus.Starting)
            ready  <- Deferred[F, Either[Throwable, Unit]]
            closed <- Ref.of[F, Boolean](false)
            close = closed
              .modify:
                case true  => true -> F.unit
                case false => true -> release
              .flatten
          yield PreviewSession(
            workspace,
            command,
            policy,
            running,
            status,
            ready,
            close,
          )

  private def monitor(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Unit] =
    val startup = awaitReady(session.command, session.policy).attempt
      .race(session.running.awaitExit.attempt)
    startup
      .flatMap:
        case Left(Right(_))     => onReady(workspace, session)
        case Left(Left(error))  => failStartup(workspace, session, error)
        case Right(Right(exit)) =>
          val failure = PreviewExited(session.command.url, exit)
          failStartup(workspace, session, failure)
        case Right(Left(error)) => failStartup(workspace, session, error)

  private def failStartup(
      workspace: Workspace,
      session: PreviewSession[F],
      error: Throwable,
  ): F[Unit] =
    updateTerminalView(
      workspace,
      session,
      PreviewTerminal.Failed(message(error)),
    ).void *>
      session.ready.complete(Left(error)).void

  private def onReady(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Unit] =
    session.status
      .modify:
        case PreviewStatus.Stopping => PreviewStatus.Stopping -> false
        case _                      => PreviewStatus.Ready    -> true
      .flatMap:
        case false => session.ready.complete(Right(())).void
        case true  =>
          ports
            .info(s"DocTrellis preview is ready for ${workspace.name}.")
            .attempt
            .void *>
            displayReady(workspace, session).flatMap: displayed =>
              session.ready.complete(Right(())).void *>
                F.whenA(displayed)(continueReadySession(workspace, session))

  private def continueReadySession(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Unit] =
    session.status.get.flatMap:
      case PreviewStatus.Stopping =>
        updateStoppedIfCurrent(workspace, session)
      case _ =>
        session.running.awaitExit.attempt.flatMap:
          case Left(error) =>
            updateTerminalView(
              workspace,
              session,
              PreviewTerminal.Failed(message(error)),
            ).flatMap: displayed =>
              F.whenA(displayed):
                report(
                  s"DocTrellis preview for ${workspace.name} failed: ${message(error)}",
                )
          case Right(exit) =>
            updateTerminalView(
              workspace,
              session,
              PreviewTerminal.Exited(exit),
            ).flatMap: displayed =>
              F.whenA(displayed):
                report(
                  s"DocTrellis preview for ${workspace.name} exited (${exit.render}).",
                )

  private def updateTerminalView(
      workspace: Workspace,
      session: PreviewSession[F],
      terminal: PreviewTerminal,
  ): F[Boolean] =
    prepareTerminalView(workspace, session, terminal).flatMap:
      case None            => F.pure(false)
      case Some(displayed) =>
        session.close.attempt.flatMap: released =>
          convergeReleasedTerminal(workspace, session, displayed)
            .flatTap(_ =>
              released.swap.toOption.traverse_(error =>
                report(
                  s"Failed to release preview for ${workspace.name}: ${message(error)}",
                ),
              ),
            )

  private def prepareTerminalView(
      workspace: Workspace,
      session: PreviewSession[F],
      terminal: PreviewTerminal,
  ): F[Option[Boolean]] =
    mutex.lock.surround:
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(session) =>
            val display = session.status.get.flatMap:
              case PreviewStatus.Stopping =>
                updateView(workspace, PreviewViewState.Stopped).as(false)
              case _ =>
                session.status.set(PreviewStatus.Terminal(terminal)) *>
                  updateView(workspace, terminal.viewState) *>
                  session.status.get.flatMap:
                    case PreviewStatus.Stopping =>
                      updateView(workspace, PreviewViewState.Stopped).as(false)
                    case _ => F.pure(true)
            display
              .flatTap(_ => removeExact(workspace.key, session))
              .map(Some(_))
          case _ => F.pure(None)

  private def convergeReleasedTerminal(
      workspace: Workspace,
      session: PreviewSession[F],
      displayed: Boolean,
  ): F[Boolean] =
    session.status.get.flatMap:
      case PreviewStatus.Stopping =>
        mutex.lock.surround:
          sessions.get.flatMap:
            _.get(workspace.key) match
              case Some(actual) if !actual.eq(session) => F.pure(false)
              case _                                   =>
                updateView(workspace, PreviewViewState.Stopped).as(false)
      case _ => F.pure(displayed)

  private def awaitReady(
      command: PreviewCommand,
      policy: PreviewPolicy,
  ): F[Unit] =
    def loop: F[Unit] =
      ports
        .probe(command.url)
        .flatMap:
          case true  => F.unit
          case false => F.sleep(policy.pollInterval.duration) *> loop

    loop.timeoutTo(
      command.startupTimeout.duration,
      F.raiseError(PreviewTimedOut(command.url, command.startupTimeout)),
    )

  private def removeExact(
      key: String,
      expected: PreviewSession[F],
  ): F[Unit] =
    sessions.update: current =>
      current.get(key) match
        case Some(actual) if actual.eq(expected) => current.removed(key)
        case _                                   => current

  private def signalStopping(key: String): F[Boolean] =
    sessions.get.flatMap:
      _.get(key) match
        case Some(session) =>
          session.status.set(PreviewStatus.Stopping).as(true)
        case None => F.pure(false)

  private def sessionView(
      session: PreviewSession[F],
      status: PreviewStatus,
  ): PreviewViewState = status match
    case PreviewStatus.Starting => PreviewViewState.Starting
    case PreviewStatus.Ready    => PreviewViewState.Ready(session.command.url)
    case PreviewStatus.Stopping => PreviewViewState.Stopped
    case PreviewStatus.Terminal(terminal) => terminal.viewState

  private def convergeOpen(
      workspace: Workspace,
      session: PreviewSession[F],
      initial: PreviewStatus,
  ): F[Unit] =
    mutex.lock.surround:
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(session) =>
            session.status.get.flatMap: current =>
              F.whenA(current != initial):
                updateView(workspace, sessionView(session, current))
          case _ => F.unit

  private def displayReady(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Boolean] =
    val current = mutex.lock.surround:
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(session) =>
            session.status.get.map(_ == PreviewStatus.Ready)
          case _ => F.pure(false)
    val display =
      if session.policy.openWhenReady then
        showView(
          workspace,
          PreviewViewState.Ready(session.command.url),
        )
      else
        updateView(
          workspace,
          PreviewViewState.Ready(session.command.url),
        )

    current.flatMap:
      case false => F.pure(false)
      case true  => display *> convergeReadyDisplay(workspace, session)

  private def convergeReadyDisplay(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Boolean] =
    mutex.lock.surround:
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(session) =>
            session.status.get.flatMap:
              case PreviewStatus.Ready => F.pure(true)
              case current             =>
                updateView(workspace, sessionView(session, current)).as(false)
          case Some(actual) =>
            actual.status.get.flatMap: current =>
              updateView(workspace, sessionView(actual, current)).as(false)
          case None =>
            session.status.get.flatMap:
              case PreviewStatus.Ready => F.pure(false)
              case current             =>
                updateView(workspace, sessionView(session, current)).as(false)

  private def updateStoppedIfCurrent(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Unit] =
    mutex.lock.surround:
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(session) =>
            updateView(workspace, PreviewViewState.Stopped)
          case _ => F.unit

  private def report(message: String): F[Unit] =
    ports.error(message).handleError(_ => ())

  private def showView(
      workspace: Workspace,
      state: PreviewViewState,
  ): F[Unit] =
    ports
      .show(workspace, state)
      .handleErrorWith(error => report(message(error)))

  private def updateView(
      workspace: Workspace,
      state: PreviewViewState,
  ): F[Unit] =
    ports
      .update(workspace, state)
      .handleErrorWith(error => report(message(error)))

  private def message(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString)

private final case class PreviewSession[F[_]](
    workspace: Workspace,
    command: PreviewCommand,
    policy: PreviewPolicy,
    running: RunningProcess[F],
    status: Ref[F, PreviewStatus],
    ready: Deferred[F, Either[Throwable, Unit]],
    close: F[Unit],
):
  def awaitReady(using F: Temporal[F]): F[Unit] =
    ready.get.flatMap(_.liftTo[F])

object PreviewService:
  def resource[F[_]](
      ports: PreviewPorts[F],
  )(using F: Temporal[F]): Resource[F, PreviewService[F]] =
    Supervisor[F](await = false).flatMap: supervisor =>
      for
        sessions <- Resource.eval(
          Ref.of[F, Map[String, PreviewSession[F]]](Map.empty),
        )
        mutex <- Resource.eval(Mutex[F])
        service = PreviewService(ports, sessions, mutex, supervisor)
        _ <- Resource.make(F.unit)(_ => service.stopAll)
      yield service
