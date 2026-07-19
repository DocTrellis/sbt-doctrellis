package org.doctrellis.vscode.application

import cats.effect.kernel.Temporal
import cats.effect.std.{Mutex, Supervisor}
import cats.effect.{Deferred, Ref, Resource}
import cats.effect.implicits.*
import cats.syntax.all.*
import org.doctrellis.vscode.domain.*

final case class RunningProcess[F[_]](
    awaitExit: F[ProcessExit],
    announcedUrl: Option[F[PreviewUrl]],
)

object RunningProcess:
  def apply[F[_]](awaitExit: F[ProcessExit]): RunningProcess[F] =
    RunningProcess(awaitExit = awaitExit, announcedUrl = None)

  def announced[F[_]](
      awaitExit: F[ProcessExit],
      awaitUrl: F[PreviewUrl],
  ): RunningProcess[F] =
    RunningProcess(awaitExit = awaitExit, announcedUrl = Some(awaitUrl))

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

final case class PreviewTimedOut(target: String, timeout: StartupTimeout)
    extends PreviewFailure(
      s"Timed out after ${timeout.duration.toMillis} ms waiting for $target.",
    )

final case class PreviewExited(target: String, exit: ProcessExit)
    extends PreviewFailure(
      s"Preview process exited before $target became ready (${exit.render}).",
    )

final case class PreviewStoppedDuringStartup(target: String)
    extends PreviewFailure(
      s"Preview startup for $target was stopped before it became ready.",
    )

final case class DynamicPreviewNotStarted(workspace: String)
    extends PreviewFailure(
      s"Start the dynamic DocTrellis preview for $workspace before opening it.",
    )

final case class DynamicPreviewStarting(workspace: String)
    extends PreviewFailure(
      s"The dynamic DocTrellis preview for $workspace is still starting; try again shortly.",
    )

final case class PreviewLaunchStopped(workspace: String)
    extends PreviewFailure(
      s"Preview startup for $workspace was stopped before process acquisition completed.",
    )

private final case class PreviewServiceClosed()
    extends IllegalStateException("The preview service is shutting down.")

final class PreviewService[F[_]] private (
    ports: PreviewPorts[F],
    sessions: Ref[F, Map[String, PreviewSession[F]]],
    workspaceMutexes: WorkspaceMutexes[F],
    lifecycle: Mutex[F],
    closing: Ref[F, Boolean],
    launches: Ref[F, Set[PendingLaunch[F]]],
    launchChanged: Ref[F, Deferred[F, Unit]],
    supervisor: Supervisor[F],
)(using F: Temporal[F]):
  def start(workspace: Workspace): F[Unit] =
    getOrStart(workspace).flatMap(_.awaitReady)

  def stop(workspace: Workspace): F[Unit] =
    stopSession(workspace, reportMissing = true)

  def stopSilently(workspace: Workspace): F[Unit] =
    stopSession(workspace, reportMissing = false)

  private def stopSession(
      workspace: Workspace,
      reportMissing: Boolean,
  ): F[Unit] =
    F.uncancelable: poll =>
      poll(selectForStopOrCancel(workspace.key)).flatMap: selected =>
        selected.session match
          case Some(session) =>
            val stopped = PreviewStoppedDuringStartup(session.target)
            session.ready.complete(Left(stopped)).void *>
              session.close *>
              removeAndStopView(workspace, session)
          case None if selected.observedActivity =>
            updateView(workspace, PreviewViewState.Stopped)
          case None if reportMissing =>
            ports.info(
              s"No DocTrellis preview is running for ${workspace.name}.",
            )
          case None => F.unit

  def restart(workspace: Workspace): F[Unit] =
    stop(workspace) *> start(workspace)

  def open(workspace: Workspace): F[Unit] =
    sessions.get.flatMap:
      _.get(workspace.key) match
        case Some(session) =>
          session.status.get.flatMap: initial =>
            session.view(initial).flatMap(showView(workspace, _)) *>
              convergeOpen(workspace, session, initial)
        case None =>
          ports
            .load(workspace)
            .flatMap: command =>
              command.endpoint match
                case PreviewEndpoint.Fixed(url) =>
                  ports
                    .probe(url)
                    .flatMap: ready =>
                      val state =
                        if ready then PreviewViewState.Ready(url)
                        else PreviewViewState.Stopped
                      showView(workspace, state)
                case PreviewEndpoint.Announced =>
                  val failure = DynamicPreviewNotStarted(workspace.name)
                  showView(
                    workspace,
                    PreviewViewState.Failed(failure.getMessage),
                  ) *> ports.info(failure.getMessage)

  def openExternal(workspace: Workspace): F[Unit] =
    sessions.get.flatMap:
      _.get(workspace.key) match
        case Some(session) =>
          session.actualUrl.get.flatMap:
            case Some(url) => ports.openExternal(url)
            case None      =>
              session.command.endpoint match
                case PreviewEndpoint.Fixed(url) => ports.openExternal(url)
                case PreviewEndpoint.Announced  =>
                  session.status.get.flatMap:
                    case PreviewStatus.Starting =>
                      F.raiseError(DynamicPreviewStarting(workspace.name))
                    case PreviewStatus.Ready =>
                      F.raiseError(
                        IllegalStateException(
                          "The dynamic preview is ready without an announced URL.",
                        ),
                      )
                    case PreviewStatus.Stopping | PreviewStatus.Terminal(_) =>
                      F.raiseError(DynamicPreviewNotStarted(workspace.name))
        case None =>
          ports
            .load(workspace)
            .flatMap:
              _.endpoint match
                case PreviewEndpoint.Fixed(url) => ports.openExternal(url)
                case PreviewEndpoint.Announced  =>
                  F.raiseError(DynamicPreviewNotStarted(workspace.name))

  def stopAll: F[Unit] =
    F.uncancelable: _ =>
      selectAllForStop.flatMap: selection =>
        val stopSelected: PreviewSession[F] => F[Unit] = session =>
          val stopped = PreviewStoppedDuringStartup(session.target)
          (session.ready.complete(Left(stopped)).void *>
            session.close *>
            removeAndStopView(session.workspace, session))
            .handleErrorWith(error =>
              report(
                s"Failed to stop preview at ${session.target}: ${message(error)}",
              ),
            )
        val stopLaunch: PendingLaunch[F] => F[Unit] =
          launch =>
            launch.cancel.complete(PreviewServiceClosed()).void *>
              launch.completed.get
        val stopSessions = selection.sessions.parTraverse_(stopSelected)
        val stopLaunches = selection.launches.parTraverse_(stopLaunch)
        (stopSessions, stopLaunches).parTupled.void

  private def getOrStart(workspace: Workspace): F[PreviewSession[F]] =
    workspaceMutexes
      .surround(workspace.key):
        ensureOpen *>
          sessions.get.flatMap:
            _.get(workspace.key) match
              case Some(session) =>
                session.status.get.flatMap:
                  case PreviewStatus.Starting | PreviewStatus.Ready =>
                    ports
                      .info(
                        s"DocTrellis preview is already active for ${workspace.name}.",
                      )
                      .as(StartDecision.Use(session))
                  case PreviewStatus.Stopping | PreviewStatus.Terminal(_) =>
                    session.isClosed.flatMap:
                      case true =>
                        removeExact(workspace.key, session) *>
                          createSession(workspace).map(StartDecision.Use(_))
                      case false => F.pure(StartDecision.Wait(session))
              case None => createSession(workspace).map(StartDecision.Use(_))
      .flatMap:
        case StartDecision.Use(session)  => F.pure(session)
        case StartDecision.Wait(session) =>
          session.awaitClosed *> getOrStart(workspace)

  private def createSession(workspace: Workspace): F[PreviewSession[F]] =
    (
      Deferred[F, Throwable],
      Deferred[F, Unit],
      Deferred[F, Unit],
    ).tupled.flatMap: (cancel, completed, nextChange) =>
      val pending = PendingLaunch(
        workspaceKey = workspace.key,
        workspaceName = workspace.name,
        cancel = cancel,
        completed = completed,
      )
      val register = lifecycle.lock.surround:
        closing.get.flatMap:
          case true  => F.raiseError(PreviewServiceClosed())
          case false =>
            launches.update(_ + pending) *>
              launchChanged
                .getAndSet(nextChange)
                .flatMap(_.complete(()).void)
      val unregister = lifecycle.lock.surround:
        launches.update(_ - pending) *> completed.complete(()).void
      val allocate = for
        command <- ports.load(workspace)
        policy  <- ports.policy(workspace)
        session <- installSession(workspace, command, policy)
      yield session
      val run = allocate
        .race(cancel.get)
        .flatMap:
          case Left(session) => F.pure(session)
          case Right(error)  => F.raiseError(error)
        .handleErrorWith:
          case error: PreviewServiceClosed =>
            updateView(workspace, PreviewViewState.Stopped) *>
              F.raiseError(error)
          case error: PreviewLaunchStopped =>
            updateView(workspace, PreviewViewState.Stopped) *>
              F.raiseError(error)
          case error =>
            updateView(
              workspace,
              PreviewViewState.Failed(message(error)),
            ) *> F.raiseError(error)
      (register *> run).guarantee(unregister)

  private def installSession(
      workspace: Workspace,
      command: PreviewCommand,
      policy: PreviewPolicy,
  ): F[PreviewSession[F]] =
    F.uncancelable: poll =>
      poll(allocateSession(workspace, command, policy)).flatMap: session =>
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
            session.close.attempt.flatMap:
              case Right(_)    => removeExactLocked(workspace.key, session)
              case Left(error) =>
                report(
                  s"Failed to release preview for ${workspace.name}: ${message(error)}",
                ),
          )
        val install = lifecycle.lock.surround:
          closing.get.flatMap:
            case true  => F.raiseError(PreviewServiceClosed())
            case false => sessions.update(_.updated(workspace.key, session))
        val publish = install *>
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
    F.uncancelable: poll =>
      poll(ports.launch(workspace, command).allocated).flatMap:
        case (running, release) =>
          val capture = for
            status    <- Ref.of[F, PreviewStatus](PreviewStatus.Starting)
            ready     <- Deferred[F, Either[Throwable, PreviewUrl]]
            actualUrl <- Ref.of[F, Option[PreviewUrl]](None)
            closer    <- SessionCloser.create(release)
          yield PreviewSession(
            workspace = workspace,
            command = command,
            policy = policy,
            running = running,
            status = status,
            ready = ready,
            actualUrl = actualUrl,
            closer = closer,
          )
          capture.onError(_ => release.attempt.void)

  private def monitor(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Unit] =
    val startup = awaitReady(session).attempt
      .race(session.running.awaitExit.attempt)
    startup
      .flatMap:
        case Left(Right(url))   => onReady(workspace, session, url)
        case Left(Left(error))  => failStartup(workspace, session, error)
        case Right(Right(exit)) =>
          val failure = PreviewExited(session.target, exit)
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
      url: PreviewUrl,
  ): F[Unit] =
    session.actualUrl.set(Some(url)) *>
      session.status
        .modify:
          case PreviewStatus.Stopping => PreviewStatus.Stopping -> false
          case _                      => PreviewStatus.Ready    -> true
        .flatMap:
          case false =>
            session.ready
              .complete(Left(PreviewStoppedDuringStartup(session.target)))
              .void
          case true =>
            session.ready.complete(Right(url)).void *>
              ports
                .info(s"DocTrellis preview is ready for ${workspace.name}.")
                .attempt
                .void *>
              displayReady(workspace, session, url).flatMap: displayed =>
                session.status.get.flatMap:
                  case PreviewStatus.Ready =>
                    F.whenA(displayed)(
                      continueReadySession(workspace, session),
                    )
                  case PreviewStatus.Stopping =>
                    session.ready
                      .complete(
                        Left(PreviewStoppedDuringStartup(session.target)),
                      )
                      .void
                  case PreviewStatus.Starting | PreviewStatus.Terminal(_) =>
                    session.ready
                      .complete(
                        Left(PreviewStoppedDuringStartup(session.target)),
                      )
                      .void

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
          released match
            case Right(_) =>
              removeExactLocked(workspace.key, session) *>
                convergeReleasedTerminal(workspace, session, displayed)
            case Left(error) =>
              report(
                s"Failed to release preview for ${workspace.name}: ${message(error)}",
              ).as(false)

  private def prepareTerminalView(
      workspace: Workspace,
      session: PreviewSession[F],
      terminal: PreviewTerminal,
  ): F[Option[Boolean]] =
    workspaceMutexes.surround(workspace.key):
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
              .map(Some(_))
          case _ => F.pure(None)

  private def convergeReleasedTerminal(
      workspace: Workspace,
      session: PreviewSession[F],
      displayed: Boolean,
  ): F[Boolean] =
    session.status.get.flatMap:
      case PreviewStatus.Stopping =>
        workspaceMutexes.surround(workspace.key):
          sessions.get.flatMap:
            _.get(workspace.key) match
              case Some(actual) if !actual.eq(session) => F.pure(false)
              case _                                   =>
                updateView(workspace, PreviewViewState.Stopped).as(false)
      case _ => F.pure(displayed)

  private def awaitReady(session: PreviewSession[F]): F[PreviewUrl] =
    def loop(url: PreviewUrl): F[PreviewUrl] =
      ports
        .probe(url)
        .flatMap:
          case true  => F.pure(url)
          case false =>
            F.sleep(session.policy.pollInterval.duration) *> loop(url)

    announcedUrl(session)
      .flatMap(loop)
      .timeoutTo(
        session.command.startupTimeout.duration,
        F.raiseError(
          PreviewTimedOut(session.target, session.command.startupTimeout),
        ),
      )

  private def announcedUrl(session: PreviewSession[F]): F[PreviewUrl] =
    session.command.endpoint match
      case PreviewEndpoint.Fixed(url) => F.pure(url)
      case PreviewEndpoint.Announced  =>
        session.running.announcedUrl match
          case Some(awaitUrl) => awaitUrl
          case None           =>
            F.raiseError(
              IllegalStateException(
                "The dynamic preview process did not provide an IPC announcement.",
              ),
            )

  private def removeExact(
      key: String,
      expected: PreviewSession[F],
  ): F[Unit] =
    sessions.update: current =>
      current.get(key) match
        case Some(actual) if actual.eq(expected) => current.removed(key)
        case _                                   => current

  private def selectForStop(key: String): F[Option[PreviewSession[F]]] =
    workspaceMutexes.surround(key):
      sessions.get.flatMap:
        _.get(key).traverse: session =>
          session.status.set(PreviewStatus.Stopping).as(session)

  private def selectForStopOrCancel(key: String): F[StopLookup[F]] =
    def loop(observedActivity: Boolean): F[StopLookup[F]] =
      sessions.get.flatMap: currentSessions =>
        val observed = observedActivity || currentSessions.contains(key)
        lifecycle.lock
          .surround((launches.get, launchChanged.get).tupled)
          .flatMap: (activeLaunches, changed) =>
            activeLaunches.find(_.workspaceKey == key) match
              case Some(launch) =>
                launch.cancel
                  .complete(PreviewLaunchStopped(launch.workspaceName))
                  .void *>
                  launch.completed.get *>
                  loop(observedActivity = true)
              case None =>
                selectForStop(key)
                  .race(changed.get)
                  .flatMap:
                    case Left(session) =>
                      F.pure(StopLookup(session, observed))
                    case Right(_) => loop(observed)

    loop(observedActivity = false)

  private def selectAllForStop: F[StopSelection[F]] =
    lifecycle.lock
      .surround:
        closing.set(true) *> (sessions.get, launches.get).tupled
      .flatMap: (selected, activeLaunches) =>
        selected.values.toList
          .parTraverse_(session => session.status.set(PreviewStatus.Stopping))
          .as(StopSelection(selected.values.toList, activeLaunches.toList))

  private def removeExactLocked(
      key: String,
      expected: PreviewSession[F],
  ): F[Unit] =
    workspaceMutexes.surround(key)(removeExact(key, expected))

  private def removeAndStopView(
      workspace: Workspace,
      expected: PreviewSession[F],
  ): F[Unit] =
    workspaceMutexes.surround(workspace.key):
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(expected) =>
            removeExact(workspace.key, expected) *>
              updateView(workspace, PreviewViewState.Stopped)
          case None    => updateView(workspace, PreviewViewState.Stopped)
          case Some(_) => F.unit

  private def convergeOpen(
      workspace: Workspace,
      session: PreviewSession[F],
      initial: PreviewStatus,
  ): F[Unit] =
    workspaceMutexes.surround(workspace.key):
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(session) =>
            session.status.get.flatMap: current =>
              F.whenA(current != initial):
                session.view(current).flatMap(updateView(workspace, _))
          case _ => F.unit

  private def displayReady(
      workspace: Workspace,
      session: PreviewSession[F],
      url: PreviewUrl,
  ): F[Boolean] =
    val current = workspaceMutexes.surround(workspace.key):
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(session) =>
            session.status.get.map(_ == PreviewStatus.Ready)
          case _ => F.pure(false)
    val display =
      if session.policy.openWhenReady then
        showView(
          workspace,
          PreviewViewState.Ready(url),
        )
      else
        updateView(
          workspace,
          PreviewViewState.Ready(url),
        )

    current.flatMap:
      case false => F.pure(false)
      case true  => display *> convergeReadyDisplay(workspace, session)

  private def convergeReadyDisplay(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Boolean] =
    workspaceMutexes.surround(workspace.key):
      sessions.get.flatMap:
        _.get(workspace.key) match
          case Some(actual) if actual.eq(session) =>
            session.status.get.flatMap:
              case PreviewStatus.Ready => F.pure(true)
              case current             =>
                session
                  .view(current)
                  .flatMap(updateView(workspace, _))
                  .as(false)
          case Some(actual) =>
            actual.status.get.flatMap: current =>
              actual.view(current).flatMap(updateView(workspace, _)).as(false)
          case None =>
            session.status.get.flatMap:
              case PreviewStatus.Ready => F.pure(false)
              case current             =>
                session
                  .view(current)
                  .flatMap(updateView(workspace, _))
                  .as(false)

  private def updateStoppedIfCurrent(
      workspace: Workspace,
      session: PreviewSession[F],
  ): F[Unit] =
    workspaceMutexes.surround(workspace.key):
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

  private def ensureOpen: F[Unit] =
    lifecycle.lock.surround:
      closing.get.flatMap(value => F.raiseWhen(value)(PreviewServiceClosed()))

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
    ready: Deferred[F, Either[Throwable, PreviewUrl]],
    actualUrl: Ref[F, Option[PreviewUrl]],
    closer: SessionCloser[F],
):
  def awaitReady(using F: Temporal[F]): F[Unit] =
    ready.get.flatMap(_.liftTo[F]).void

  def target: String = command.endpoint.description

  def view(status: PreviewStatus)(using F: Temporal[F]): F[PreviewViewState] =
    status match
      case PreviewStatus.Starting => F.pure(PreviewViewState.Starting)
      case PreviewStatus.Ready    =>
        actualUrl.get.map:
          case Some(url) => PreviewViewState.Ready(url)
          case None      =>
            PreviewViewState.Failed(
              "The preview became ready without an announced URL.",
            )
      case PreviewStatus.Stopping           => F.pure(PreviewViewState.Stopped)
      case PreviewStatus.Terminal(terminal) => F.pure(terminal.viewState)

  def close: F[Unit] = closer.close

  def awaitClosed: F[Unit] = closer.awaitClosed

  def isClosed: F[Boolean] = closer.isClosed

private final class SessionCloser[F[_]] private (
    release: F[Unit],
    state: Ref[F, SessionCloseState[F]],
    started: Deferred[F, Unit],
)(using F: Temporal[F]):
  def close: F[Unit] =
    F.uncancelable: poll =>
      Deferred[F, Either[Throwable, Unit]].flatMap: candidate =>
        state
          .modify:
            case SessionCloseState.Fresh() | SessionCloseState.Failed() =>
              SessionCloseState.Running(candidate) ->
                SessionCloseDecision.Owner(candidate)
            case current @ SessionCloseState.Running(result) =>
              current -> SessionCloseDecision.Wait(result)
            case SessionCloseState.Closed() =>
              SessionCloseState.Closed() -> SessionCloseDecision.Done()
          .flatMap:
            case SessionCloseDecision.Owner(result) =>
              started.complete(()).void *>
                release.attempt.flatMap: outcome =>
                  val finish = outcome match
                    case Right(_) =>
                      state.set(SessionCloseState.Closed())
                    case Left(_) => state.set(SessionCloseState.Failed())
                  finish *>
                    result.complete(outcome).void *>
                    outcome.liftTo[F]
            case SessionCloseDecision.Wait(result) =>
              poll(result.get).flatMap(_.liftTo[F])
            case SessionCloseDecision.Done() => F.unit

  def awaitClosed: F[Unit] =
    state.get.flatMap:
      case SessionCloseState.Fresh()         => started.get *> awaitClosed
      case SessionCloseState.Running(result) =>
        result.get.flatMap:
          case Right(_) => F.unit
          case Left(_)  => close
      case SessionCloseState.Failed() => close
      case SessionCloseState.Closed() => F.unit

  def isClosed: F[Boolean] =
    state.get.map:
      case SessionCloseState.Closed() => true
      case _                          => false

private object SessionCloser:
  def create[F[_]](
      release: F[Unit],
  )(using F: Temporal[F]): F[SessionCloser[F]] =
    for
      state   <- Ref.of[F, SessionCloseState[F]](SessionCloseState.Fresh())
      started <- Deferred[F, Unit]
    yield SessionCloser(
      release = release,
      state = state,
      started = started,
    )

private enum SessionCloseState[F[_]]:
  case Fresh()
  case Failed()
  case Closed()
  case Running(result: Deferred[F, Either[Throwable, Unit]])

private enum SessionCloseDecision[F[_]]:
  case Owner(result: Deferred[F, Either[Throwable, Unit]])
  case Wait(result: Deferred[F, Either[Throwable, Unit]])
  case Done()

private enum StartDecision[F[_]]:
  case Use(session: PreviewSession[F])
  case Wait(session: PreviewSession[F])

private final case class StopSelection[F[_]](
    sessions: List[PreviewSession[F]],
    launches: List[PendingLaunch[F]],
)

private final case class PendingLaunch[F[_]](
    workspaceKey: String,
    workspaceName: String,
    cancel: Deferred[F, Throwable],
    completed: Deferred[F, Unit],
)

private final case class StopLookup[F[_]](
    session: Option[PreviewSession[F]],
    observedActivity: Boolean,
)

private final class WorkspaceMutexes[F[_]](
    locks: Ref[F, Map[String, Mutex[F]]],
)(using F: Temporal[F]):
  def surround[A](key: String)(action: F[A]): F[A] =
    mutex(key).flatMap(_.lock.surround(action))

  private def mutex(key: String): F[Mutex[F]] =
    locks.get.flatMap:
      _.get(key) match
        case Some(existing) => F.pure(existing)
        case None           =>
          Mutex[F].flatMap: candidate =>
            locks.modify: current =>
              current.get(key) match
                case Some(existing) => current -> existing
                case None => current.updated(key, candidate) -> candidate

private object WorkspaceMutexes:
  def create[F[_]](using F: Temporal[F]): F[WorkspaceMutexes[F]] =
    Ref
      .of[F, Map[String, Mutex[F]]](Map.empty)
      .map(WorkspaceMutexes(_))

object PreviewService:
  def resource[F[_]](
      ports: PreviewPorts[F],
  )(using F: Temporal[F]): Resource[F, PreviewService[F]] =
    Supervisor[F](await = false).flatMap: supervisor =>
      for
        sessions <- Resource.eval(
          Ref.of[F, Map[String, PreviewSession[F]]](Map.empty),
        )
        workspaceMutexes <- Resource.eval(WorkspaceMutexes.create[F])
        lifecycle        <- Resource.eval(Mutex[F])
        closing          <- Resource.eval(Ref.of[F, Boolean](false))
        launches         <- Resource.eval(
          Ref.of[F, Set[PendingLaunch[F]]](Set.empty),
        )
        initialLaunchChange <- Resource.eval(Deferred[F, Unit])
        launchChanged       <- Resource.eval(
          Ref.of[F, Deferred[F, Unit]](initialLaunchChange),
        )
        service = PreviewService(
          ports = ports,
          sessions = sessions,
          workspaceMutexes = workspaceMutexes,
          lifecycle = lifecycle,
          closing = closing,
          launches = launches,
          launchChanged = launchChanged,
          supervisor = supervisor,
        )
        _ <- Resource.make(F.unit)(_ => service.stopAll)
      yield service
