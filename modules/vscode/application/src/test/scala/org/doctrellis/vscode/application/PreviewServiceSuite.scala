package org.doctrellis.vscode.application

import scala.concurrent.duration.*

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*

import munit.CatsEffectSuite

import org.doctrellis.vscode.domain.*

class PreviewServiceSuite extends CatsEffectSuite:
  private val workspace =
    Workspace("file:///workspace", "example", "/workspace", "file:///workspace")

  private val command =
    PreviewCommand(
      right(Executable.parse("sbt")),
      Vector(right(ProcessArgument.parse("doctrellisPreview", 0))),
      right(WorkingDirectory.parseResolved("/workspace", "", "/", false)),
      PreviewEndpoint.Fixed(
        right(PreviewUrl.parse("http://127.0.0.1:4242")),
      ),
      right(StartupTimeout.parse(90000)),
    )

  private val policy = PreviewPolicy(false, right(PollInterval.parse(500)))

  test("starts at most one process for a workspace"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      algebra = healthyAlgebra(launches, releases)
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *> service.start(workspace) *> launches.get
            .map(assertEquals(_, 1))
      released <- releases.get
    yield assertEquals(released, 1)

  test("serializes concurrent starts for the same workspace"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      algebra = healthyAlgebra(launches, releases)
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          List.fill(8)(service.start(workspace)).parSequence_ *> launches.get
            .map(assertEquals(_, 1))
      released <- releases.get
    yield assertEquals(released, 1)

  test("a slow launch does not block a different workspace"):
    val second = workspace.copy(
      key = "file:///workspace-two",
      name = "example-two",
      root = "/workspace-two",
      uri = "file:///workspace-two",
    )
    for
      firstEntered  <- Deferred[IO, Unit]
      continueFirst <- Deferred[IO, Unit]
      secondEntered <- Deferred[IO, Unit]
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (current, _) =>
          Resource.eval:
            if current.key == workspace.key then
              firstEntered.complete(()).void *>
                continueFirst.get.as(RunningProcess(IO.never))
            else secondEntered.complete(()).as(RunningProcess(IO.never))
        ,
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            first  <- service.start(workspace).start
            _      <- firstEntered.get
            second <- service.start(second).start
            _      <- secondEntered.get
              .timeout(1.second)
              .guarantee(continueFirst.complete(()).void)
            _ <- first.joinWithNever
            _ <- second.joinWithNever
          yield ()
    yield ()

  test("stop all cancels and waits for an in-flight launch"):
    for
      launchEntered <- Deferred[IO, Unit]
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.eval(
            launchEntered.complete(()).void *> IO.never[RunningProcess[IO]],
          ),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            starting <- service.start(workspace).attempt.start
            _        <- launchEntered.get
            _        <- service.stopAll.timeout(1.second)
            outcome  <- starting.joinWithNever.timeout(1.second)
          yield outcome
    yield result match
      case Left(PreviewServiceClosed()) => ()
      case other => fail(s"expected PreviewServiceClosed, got $other")

  test("explicit stop cancels an in-flight launch for its workspace"):
    for
      launchEntered <- Deferred[IO, Unit]
      states        <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.eval(
            launchEntered.complete(()).void *> IO.never[RunningProcess[IO]],
          ),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, state) => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            starting <- service.start(workspace).attempt.start
            _        <- launchEntered.get
            _        <- service.stop(workspace).timeout(1.second)
            outcome  <- starting.joinWithNever.timeout(1.second)
          yield outcome
      actual <- states.get
    yield
      result match
        case Left(PreviewLaunchStopped(actual)) =>
          assertEquals(actual, workspace.name)
        case other => fail(s"expected PreviewLaunchStopped, got $other")
      assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("stop releases a process acquired at the cancellation boundary"):
    for
      launchEntered   <- Deferred[IO, Unit]
      continueAcquire <- Deferred[IO, Unit]
      released        <- Deferred[IO, Unit]
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(
            IO.uncancelable: _ =>
              launchEntered.complete(()).void *>
                continueAcquire.get.as(RunningProcess(IO.never)),
          )(_ => released.complete(()).void),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            starting <- service.start(workspace).attempt.start
            _        <- launchEntered.get
            stopping <- service.stop(workspace).start
            _        <- IO.cede
            _        <- continueAcquire.complete(())
            _        <- released.get.timeout(1.second)
            outcome  <- starting.joinWithNever.timeout(1.second)
            _        <- stopping.joinWithNever.timeout(1.second)
          yield outcome
    yield result match
      case Left(PreviewLaunchStopped(actual)) =>
        assertEquals(actual, workspace.name)
      case other => fail(s"expected PreviewLaunchStopped, got $other")

  test("cancelling a registered launch does not strand shutdown"):
    for
      loadEntered <- Deferred[IO, Unit]
      algebra = PreviewAlgebra[IO](
        load = _ => loadEntered.complete(()).void *> IO.never,
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(IO.never)),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            starting <- service.start(workspace).start
            _        <- loadEntered.get
            _        <- starting.cancel
            _        <- service.stopAll.timeout(1.second)
          yield ()
    yield ()

  test("reports an exit that happens before readiness"):
    val exit    = ProcessExit(Some(1), None, None)
    val algebra = PreviewAlgebra[IO](
      load = _ => IO.pure(command),
      policy = _ => IO.pure(policy),
      launch = (_, _) => Resource.pure(RunningProcess(IO.pure(exit))),
      probe = _ => IO.never,
      show = (_, _) => IO.unit,
      update = (_, _) => IO.unit,
      openExternal = _ => IO.unit,
      info = _ => IO.unit,
      error = _ => IO.unit,
    )
    PreviewService
      .resource(algebra)
      .use: service =>
        service
          .start(workspace)
          .attempt
          .map: result =>
            assert(result.swap.exists:
              case PreviewExited(_, _) => true
              case _                   => false)

  test("stop releases the running process"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      algebra = healthyAlgebra(launches, releases)
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *> service.stop(workspace)
      released <- releases.get
    yield assertEquals(released, 1)

  test("stop is idempotent"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      algebra = healthyAlgebra(launches, releases)
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *>
            service.stop(workspace) *>
            service.stop(workspace)
      released <- releases.get
    yield assertEquals(released, 1)

  test("concurrent stops share one in-flight release"):
    for
      closeEntered  <- Deferred[IO, Unit]
      continueClose <- Deferred[IO, Unit]
      releases      <- Ref.of[IO, Int](0)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(IO.pure(RunningProcess(IO.never)))(_ =>
            releases.update(_ + 1) *>
              closeEntered.complete(()).void *>
              continueClose.get,
          ),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      count <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _      <- service.start(workspace)
            first  <- service.stop(workspace).start
            _      <- closeEntered.get
            second <- service.stop(workspace).start
            _      <- IO.cede
            _      <- continueClose.complete(())
            _      <- (first.joinWithNever, second.joinWithNever).parTupled
            actual <- releases.get
          yield actual
    yield assertEquals(count, 1)

  test("a failed release can be retried before a replacement starts"):
    val failure = IllegalStateException("transient release failure")
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(
            launches.update(_ + 1).as(RunningProcess(IO.never)),
          )(_ =>
            releases
              .modify(count => (count + 1) -> count)
              .flatMap:
                case 0 => IO.raiseError(failure)
                case _ => IO.unit,
          ),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _         <- service.start(workspace)
            firstStop <- service.stop(workspace).attempt
            _         <- service.start(workspace)
            launched  <- launches.get
            released  <- releases.get
          yield (firstStop, launched, released)
      (firstStop, launched, released) = result
    yield
      assertEquals(firstStop, Left(failure))
      assertEquals(launched, 2)
      assertEquals(released, 2)

  test("restart waits for the previous release before launching"):
    for
      launches      <- Ref.of[IO, Int](0)
      closeEntered  <- Deferred[IO, Unit]
      continueClose <- Deferred[IO, Unit]
      ordering      <- Ref.of[IO, Vector[String]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(
            launches
              .updateAndGet(_ + 1)
              .flatTap: count =>
                IO.whenA(count == 2)(ordering.update(_ :+ "new-launch"))
              .as(RunningProcess(IO.never)),
          )(_ =>
            launches.get.flatMap: count =>
              IO.whenA(count == 1):
                closeEntered.complete(()).void *>
                  continueClose.get *>
                  ordering.update(_ :+ "old-released"),
          ),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      actual <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _       <- service.start(workspace)
            restart <- service.restart(workspace).start
            _       <- closeEntered.get
            before  <- launches.get
            _       <- continueClose.complete(())
            _       <- restart.joinWithNever
            order   <- ordering.get
          yield before -> order
      (before, order) = actual
    yield
      assertEquals(before, 1)
      assertEquals(order, Vector("old-released", "new-launch"))

  test("a completed stop cannot overwrite a replacement starting view"):
    for
      launches         <- Ref.of[IO, Int](0)
      closeEntered     <- Deferred[IO, Unit]
      continueClose    <- Deferred[IO, Unit]
      replacementSeen  <- Deferred[IO, Unit]
      replacementReady <- Deferred[IO, Unit]
      states           <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource
            .make(
              launches
                .updateAndGet(_ + 1)
                .map(count => count -> RunningProcess[IO](IO.never)),
            ):
              case (count, _) =>
                IO.whenA(count == 1):
                  closeEntered.complete(()).void *> continueClose.get
            .map(_._2),
        probe = _ =>
          launches.get.flatMap:
            case 1 => IO.pure(true)
            case _ => replacementReady.get.as(true),
        show = (_, _) => IO.unit,
        update = (_, state) =>
          states.update(_ :+ state) *>
            (state match
              case PreviewViewState.Starting =>
                launches.get.flatMap(count =>
                  IO.whenA(count == 2)(replacementSeen.complete(()).void),
                )
              case _ => IO.unit),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      actual <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _       <- service.start(workspace)
            stop    <- service.stop(workspace).start
            _       <- closeEntered.get
            replace <- service.start(workspace).start
            _       <- continueClose.complete(())
            _       <- replacementSeen.get
            _       <- stop.joinWithNever
            current <- states.get
            _       <- replacementReady.complete(())
            _       <- replace.joinWithNever
          yield current
    yield assertEquals(actual.lastOption, Some(PreviewViewState.Starting))

  test("silent workspace cleanup stops only the selected workspace"):
    val second = workspace.copy(
      key = "file:///workspace-two",
      name = "example-two",
      root = "/workspace-two",
      uri = "file:///workspace-two",
    )
    for
      releases <- Ref.of[IO, Vector[String]](Vector.empty)
      launches <- Ref.of[IO, Map[String, Int]](Map.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (current, _) =>
          Resource.make(
            launches
              .update(values =>
                values.updated(
                  current.key,
                  values.getOrElse(current.key, 0) + 1,
                ),
              )
              .as(RunningProcess(IO.never)),
          )(_ => releases.update(_ :+ current.key)),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *>
            service.start(second) *>
            service.stopSilently(workspace) *>
            service.start(second) *>
            (releases.get, launches.get).tupled
      (releasedDuringUse, launched) = result
    yield
      assertEquals(releasedDuringUse, Vector(workspace.key))
      assertEquals(launched.get(workspace.key), Some(1))
      assertEquals(launched.get(second.key), Some(1))

  test("explicit stop leaves an existing preview view in the stopped state"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      views    <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      base    = healthyAlgebra(launches, releases)
      algebra = base.copy(update = (_, state) => views.update(_ :+ state))
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *> service.stop(workspace)
      states <- views.get
    yield assertEquals(states.lastOption, Some(PreviewViewState.Stopped))

  test("unexpected exit updates an open preview view without disposing it"):
    val exit = ProcessExit(Some(7), None, None)
    for
      exited <- Deferred[IO, ProcessExit]
      shown  <- Deferred[IO, Unit]
      views  <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(exited.get)),
        probe = _ => IO.pure(true),
        show = (_, state) => views.update(_ :+ state),
        update = (_, state) =>
          views.update(_ :+ state) *>
            (state match
              case PreviewViewState.Exited(`exit`) => shown.complete(()).void
              case _                               => IO.unit),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *>
            exited.complete(exit) *>
            shown.get
      states <- views.get
    yield assert(states.contains(PreviewViewState.Exited(exit)))

  test("an exited session is replaceable while its error report is pending"):
    val exit = ProcessExit(Some(7), None, None)
    for
      launches       <- Ref.of[IO, Int](0)
      probeCalls     <- Ref.of[IO, Int](0)
      exited         <- Deferred[IO, ProcessExit]
      reportEntered  <- Deferred[IO, Unit]
      continueReport <- Deferred[IO, Unit]
      shown          <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.eval:
            launches
              .modify(count => (count + 1) -> count)
              .map:
                case 0 => RunningProcess(exited.get)
                case _ => RunningProcess(IO.never),
        probe = _ =>
          probeCalls.modify: count =>
            (count + 1) -> (count != 1),
        show = (_, state) => shown.update(_ :+ state),
        update = (_, state) => shown.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => reportEntered.complete(()).void *> continueReport.get,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _          <- service.start(workspace)
            _          <- exited.complete(exit)
            _          <- reportEntered.get
            _          <- service.open(workspace)
            afterOpen  <- shown.get
            _          <- service.start(workspace)
            _          <- service.open(workspace)
            count      <- launches.get
            afterStart <- shown.get
            _          <- continueReport.complete(())
          yield (afterOpen, count, afterStart)
      (afterOpen, count, afterStart) = result
    yield
      assertEquals(afterOpen.lastOption, Some(PreviewViewState.Stopped))
      assertEquals(count, 2)
      assertEquals(
        afterStart.lastOption,
        Some(PreviewViewState.Ready(urlOf(command))),
      )

  test("open passes the workspace and session URL to the preview view"):
    for
      shown <- Ref.of[IO, Vector[(Workspace, PreviewViewState)]](Vector.empty)
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      base    = healthyAlgebra(launches, releases)
      algebra = base.copy(show =
        (current, state) => shown.update(_ :+ (current -> state)),
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *> service.open(workspace)
      actual <- shown.get
    yield assertEquals(
      actual.lastOption,
      Some(workspace -> PreviewViewState.Ready(urlOf(command))),
    )

  test("openExternal uses the active session URL or loads the configured URL"):
    val configured = command.copy(endpoint =
      PreviewEndpoint.Fixed(
        right(PreviewUrl.parse("http://127.0.0.1:5252")),
      ),
    )
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      loads    <- Ref.of[IO, Int](0)
      opened   <- Ref.of[IO, Vector[PreviewUrl]](Vector.empty)
      base    = healthyAlgebra(launches, releases)
      algebra = base.copy(
        load = _ =>
          loads.modify: count =>
            (count + 1) -> (if count == 0 then command else configured),
        openExternal = url => opened.update(_ :+ url),
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *>
            service.openExternal(workspace) *>
            service.stop(workspace) *>
            service.openExternal(workspace)
      actual <- opened.get
    yield assertEquals(actual, Vector(urlOf(command), urlOf(configured)))

  test("openExternal keeps the fixed URL while its session is starting"):
    for
      probeEntered  <- Deferred[IO, Unit]
      continueProbe <- Deferred[IO, Unit]
      launches      <- Ref.of[IO, Int](0)
      releases      <- Ref.of[IO, Int](0)
      opened        <- Ref.of[IO, Vector[PreviewUrl]](Vector.empty)
      base    = healthyAlgebra(launches, releases)
      algebra = base.copy(
        probe =
          _ => probeEntered.complete(()).void *> continueProbe.get.as(true),
        openExternal = url => opened.update(_ :+ url),
      )
      actual <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            starting <- service.start(workspace).start
            _        <- probeEntered.get
            _        <- service.openExternal(workspace)
            result   <- opened.get
            _        <- continueProbe.complete(())
            _        <- starting.joinWithNever
          yield result
    yield assertEquals(actual, Vector(urlOf(command)))

  test("openExternal reports an announced preview that is still starting"):
    val dynamic = command.copy(endpoint = PreviewEndpoint.Announced)
    for
      launched <- Deferred[IO, Unit]
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(dynamic),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.eval(
            launched
              .complete(())
              .as(
                RunningProcess.announced(
                  awaitExit = IO.never,
                  awaitUrl = IO.never,
                ),
              ),
          ),
        probe = _ => IO.pure(false),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            starting <- service.start(workspace).attempt.start
            _        <- launched.get
            external <- service.openExternal(workspace).attempt
            _        <- service.stop(workspace)
            _        <- starting.joinWithNever
          yield external
    yield result match
      case Left(DynamicPreviewStarting(actual)) =>
        assertEquals(actual, workspace.name)
      case other => fail(s"expected DynamicPreviewStarting, got $other")

  test("a ready signal preserves a session that already became terminal"):
    val exit     = ProcessExit(code = Some(17), signal = None, error = None)
    val terminal = PreviewStatus.Terminal(PreviewTerminal.Exited(exit))
    for
      status     <- Ref.of[IO, PreviewStatus](terminal)
      ready      <- Deferred[IO, Either[Throwable, PreviewUrl]]
      acceptance <- PreviewReadyGate.accept(
        status,
        ready,
        command.endpoint.description,
        urlOf(command),
      )
      actualStatus <- status.get
      completion   <- ready.get
    yield
      assertEquals(acceptance, ReadyAcceptance.Rejected)
      assertEquals(actualStatus, terminal)
      completion match
        case Left(PreviewStoppedDuringStartup(actual)) =>
          assertEquals(actual, command.endpoint.description)
        case other => fail(s"expected PreviewStoppedDuringStartup, got $other")

  test("openExternal rejects a stopping announced session"):
    val dynamic   = command.copy(endpoint = PreviewEndpoint.Announced)
    val announced = right(PreviewUrl.parse("http://127.0.0.1:53117"))
    for
      releaseEntered  <- Deferred[IO, Unit]
      continueRelease <- Deferred[IO, Unit]
      opened          <- Ref.of[IO, Vector[PreviewUrl]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(dynamic),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(
            IO.pure(
              RunningProcess.announced(
                awaitExit = IO.never,
                awaitUrl = IO.pure(announced),
              ),
            ),
          )(_ => releaseEntered.complete(()).void *> continueRelease.get),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = url => opened.update(_ :+ url),
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _        <- service.start(workspace)
            stopping <- service.stop(workspace).start
            _        <- releaseEntered.get
            external <- service.openExternal(workspace).attempt
            _        <- continueRelease.complete(())
            _        <- stopping.joinWithNever
          yield external
        .guarantee(continueRelease.complete(()).void)
      actualOpened <- opened.get
    yield
      result match
        case Left(DynamicPreviewNotStarted(actual)) =>
          assertEquals(actual, workspace.name)
        case other => fail(s"expected DynamicPreviewNotStarted, got $other")
      assertEquals(actualOpened, Vector.empty)

  test("openExternal rejects a terminal announced session"):
    val dynamic   = command.copy(endpoint = PreviewEndpoint.Announced)
    val announced = right(PreviewUrl.parse("http://127.0.0.1:53118"))
    val exit      = ProcessExit(code = Some(0), signal = None, error = None)
    for
      exited          <- Deferred[IO, ProcessExit]
      releaseEntered  <- Deferred[IO, Unit]
      continueRelease <- Deferred[IO, Unit]
      opened          <- Ref.of[IO, Vector[PreviewUrl]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(dynamic),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(
            IO.pure(
              RunningProcess.announced(
                awaitExit = exited.get,
                awaitUrl = IO.pure(announced),
              ),
            ),
          )(_ => releaseEntered.complete(()).void *> continueRelease.get),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = url => opened.update(_ :+ url),
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _        <- service.start(workspace)
            _        <- exited.complete(exit)
            _        <- releaseEntered.get
            external <- service.openExternal(workspace).attempt
            _        <- continueRelease.complete(())
          yield external
        .guarantee(continueRelease.complete(()).void)
      actualOpened <- opened.get
    yield
      result match
        case Left(DynamicPreviewNotStarted(actual)) =>
          assertEquals(actual, workspace.name)
        case other => fail(s"expected DynamicPreviewNotStarted, got $other")
      assertEquals(actualOpened, Vector.empty)

  test("open without an active session reports a stopped preview"):
    for
      shown  <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      probes <- Ref.of[IO, Int](0)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(IO.never)),
        probe = _ => probes.update(_ + 1).as(false),
        show = (_, state) => shown.update(_ :+ state),
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _      <- PreviewService.resource(algebra).use(_.open(workspace))
      count  <- probes.get
      states <- shown.get
    yield
      assertEquals(count, 1)
      assertEquals(states, Vector(PreviewViewState.Stopped))

  test("open shows starting while an active session awaits readiness"):
    for
      launched <- Deferred[IO, Unit]
      shown    <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.eval(
            launched.complete(()).as(RunningProcess(IO.never)),
          ),
        probe = _ => IO.never,
        show = (_, state) => shown.update(_ :+ state),
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            start  <- service.start(workspace).attempt.start
            _      <- launched.get
            _      <- service.open(workspace)
            _      <- service.stop(workspace)
            result <- start.joinWithNever
            _      <- IO(assertStoppedDuringStartup(result))
          yield ()
      states <- shown.get
    yield assertEquals(states, Vector(PreviewViewState.Starting))

  test("open shows stopped while an active session is stopping"):
    val exit = ProcessExit(None, Some("SIGTERM"), None)
    for
      closeEntered  <- Deferred[IO, Unit]
      continueClose <- Deferred[IO, Unit]
      exited        <- Deferred[IO, ProcessExit]
      shown         <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(IO.pure(RunningProcess(exited.get)))(_ =>
            closeEntered.complete(()).void *>
              continueClose.get *>
              exited.complete(exit).void,
          ),
        probe = _ => IO.pure(true),
        show = (_, state) => shown.update(_ :+ state),
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _    <- service.start(workspace)
            stop <- service.stop(workspace).start
            _    <- closeEntered.get
            open <- service.open(workspace).start
            _    <- continueClose.complete(())
            _    <- stop.joinWithNever
            _    <- open.joinWithNever
          yield ()
      states <- shown.get
    yield assertEquals(states, Vector(PreviewViewState.Stopped))

  test("open racing a completed stop converges the panel to stopped"):
    for
      showEntered  <- Deferred[IO, Unit]
      continueShow <- Deferred[IO, Unit]
      states       <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      launches     <- Ref.of[IO, Int](0)
      releases     <- Ref.of[IO, Int](0)
      base    = healthyAlgebra(launches, releases)
      algebra = base.copy(
        show = (_, state) =>
          state match
            case PreviewViewState.Ready(_) =>
              states.update(_ :+ state) *>
                showEntered.complete(()).void *>
                continueShow.get *>
                IO.unit
            case _ => states.update(_ :+ state),
        update = (_, state) => states.update(_ :+ state),
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _    <- service.start(workspace)
            open <- service.open(workspace).start
            _    <- showEntered.get
            _    <- service.stop(workspace)
            _    <- continueShow.complete(())
            _    <- open.joinWithNever
          yield ()
      actual <- states.get
    yield assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("stop after open reads ready still owns the final panel state"):
    for
      launched       <- Deferred[IO, Unit]
      ready          <- Deferred[IO, Unit]
      showEntered    <- Deferred[IO, Unit]
      continueShow   <- Deferred[IO, Unit]
      updateEntered  <- Deferred[IO, Unit]
      continueUpdate <- Deferred[IO, Unit]
      readyUpdates   <- Ref.of[IO, Int](0)
      states         <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.eval(launched.complete(()).as(RunningProcess(IO.never))),
        probe = _ => ready.get.as(true),
        show = (_, state) =>
          state match
            case PreviewViewState.Starting =>
              showEntered.complete(()).void *>
                continueShow.get *>
                states.update(_ :+ state)
            case _ => states.update(_ :+ state),
        update = (_, state) =>
          state match
            case PreviewViewState.Ready(_) =>
              readyUpdates
                .modify(count => (count + 1) -> count)
                .flatMap:
                  case 0 => states.update(_ :+ state)
                  case _ =>
                    updateEntered.complete(()).void *>
                      continueUpdate.get *>
                      states.update(_ :+ state)
            case _ => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            start <- service.start(workspace).start
            _     <- launched.get
            open  <- service.open(workspace).start
            _     <- showEntered.get
            _     <- ready.complete(())
            _     <- start.joinWithNever
            _     <- continueShow.complete(())
            _     <- updateEntered.get
            stop  <- service.stop(workspace).start
            _     <- IO.cede
            _     <- continueUpdate.complete(())
            _     <- open.joinWithNever
            _     <- stop.joinWithNever
          yield ()
      actual <- states.get
    yield assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("an old open cannot overwrite a replacement session"):
    val replacement = command.copy(endpoint =
      PreviewEndpoint.Fixed(
        right(PreviewUrl.parse("http://127.0.0.1:5252")),
      ),
    )
    for
      loads            <- Ref.of[IO, Int](0)
      showEntered      <- Deferred[IO, Unit]
      continueShow     <- Deferred[IO, Unit]
      replacementReady <- Deferred[IO, Unit]
      states           <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ =>
          loads.modify: count =>
            (count + 1) -> (if count == 0 then command else replacement),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(IO.never)),
        probe = _ => IO.pure(true),
        show = (_, state) =>
          states.update(_ :+ state) *>
            (state match
              case PreviewViewState.Ready(url) if url == urlOf(command) =>
                showEntered.complete(()).void *> continueShow.get
              case _ => IO.unit),
        update = (_, state) =>
          states.update(_ :+ state) *>
            (state match
              case PreviewViewState.Ready(url) if url == urlOf(replacement) =>
                replacementReady.complete(()).void
              case _ => IO.unit),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      actual <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _       <- service.start(workspace)
            open    <- service.open(workspace).start
            _       <- showEntered.get
            restart <- service.restart(workspace).start
            _       <- replacementReady.get
            _       <- continueShow.complete(())
            _       <- open.joinWithNever
            _       <- restart.joinWithNever
            current <- states.get
          yield current
    yield assertEquals(
      actual.lastOption,
      Some(PreviewViewState.Ready(urlOf(replacement))),
    )

  test("open-when-ready shows the ready session in its workspace panel"):
    for
      shown <- Ref.of[IO, Vector[(Workspace, PreviewViewState)]](Vector.empty)
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      base    = healthyAlgebra(launches, releases)
      algebra = base.copy(
        policy = _ => IO.pure(policy.copy(openWhenReady = true)),
        show = (current, state) => shown.update(_ :+ (current -> state)),
      )
      _      <- PreviewService.resource(algebra).use(_.start(workspace))
      actual <- shown.get
    yield assertEquals(
      actual.lastOption,
      Some(workspace -> PreviewViewState.Ready(urlOf(command))),
    )

  test("configuration or launch failure updates an existing workspace panel"):
    val failure = IllegalArgumentException("broken config <value>")
    for
      states <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.raiseError(failure),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(IO.never)),
        probe = _ => IO.pure(false),
        show = (_, _) => IO.unit,
        update = (_, state) => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService.resource(algebra).use(_.start(workspace).attempt)
      actual <- states.get
    yield
      assertEquals(result, Left(failure))
      assertEquals(
        actual.lastOption,
        Some(PreviewViewState.Failed("broken config <value>")),
      )

  test("stop settles a start that is still waiting for readiness"):
    for
      launched <- Deferred[IO, Unit]
      released <- Ref.of[IO, Int](0)
      states   <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(
            launched.complete(()).as(RunningProcess(IO.never)),
          )(_ => released.update(_ + 1)),
        probe = _ => IO.never,
        show = (_, _) => IO.unit,
        update = (_, state) => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            start  <- service.start(workspace).attempt.start
            _      <- launched.get
            _      <- service.stop(workspace)
            result <- start.joinWithNever
            _      <- IO(assertStoppedDuringStartup(result))
          yield ()
      count  <- released.get
      actual <- states.get
    yield
      assertEquals(count, 1)
      assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("ready completion does not wait for panel display"):
    val exit = ProcessExit(None, Some("SIGTERM"), None)
    for
      infoEntered  <- Deferred[IO, Unit]
      continueInfo <- Deferred[IO, Unit]
      exited       <- Deferred[IO, ProcessExit]
      states       <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy.copy(openWhenReady = true)),
        launch = (_, _) =>
          Resource.make(IO.pure(RunningProcess(exited.get)))(_ =>
            exited.complete(exit).void,
          ),
        probe = _ => IO.pure(true),
        show = (_, state) => states.update(_ :+ state),
        update = (_, state) => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => infoEntered.complete(()).void *> continueInfo.get,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            start  <- service.start(workspace).attempt.start
            result <- start.joinWithNever.timeout(1.second)
            _      <- infoEntered.get
            _      <- service.stop(workspace)
            _      <- continueInfo.complete(())
          yield assert(result.isRight)
      actual <- states.get
    yield assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("a replaced session suppresses its old ready continuation"):
    val replacement = command.copy(endpoint =
      PreviewEndpoint.Fixed(
        right(PreviewUrl.parse("http://127.0.0.1:5252")),
      ),
    )
    for
      loads            <- Ref.of[IO, Int](0)
      infoCalls        <- Ref.of[IO, Int](0)
      infoEntered      <- Deferred[IO, Unit]
      continueInfo     <- Deferred[IO, Unit]
      replacementReady <- Deferred[IO, Unit]
      states           <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ =>
          loads.modify: count =>
            (count + 1) -> (if count == 0 then command else replacement),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(IO.never)),
        probe = _ => IO.pure(true),
        show = (_, state) => states.update(_ :+ state),
        update = (_, state) =>
          states.update(_ :+ state) *>
            (state match
              case PreviewViewState.Ready(url) if url == urlOf(replacement) =>
                replacementReady.complete(()).void
              case _ => IO.unit),
        openExternal = _ => IO.unit,
        info = _ =>
          infoCalls
            .modify(count => (count + 1) -> count)
            .flatMap:
              case 0 => infoEntered.complete(()).void *> continueInfo.get
              case _ => IO.unit,
        error = _ => IO.unit,
      )
      actual <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            firstStart <- service.start(workspace).attempt.start
            _          <- infoEntered.get
            restart    <- service.restart(workspace).start
            _          <- replacementReady.get
            _          <- continueInfo.complete(())
            first      <- firstStart.joinWithNever
            _          <- IO(assert(first.isRight))
            _          <- restart.joinWithNever
            current    <- states.get
          yield current
    yield assertEquals(
      actual.lastOption,
      Some(PreviewViewState.Ready(urlOf(replacement))),
    )

  test("stop racing a startup failure converges the panel to stopped"):
    val failure = IllegalStateException("startup probe failed")
    for
      failureEntered  <- Deferred[IO, Unit]
      continueFailure <- Deferred[IO, Unit]
      states          <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(IO.never)),
        probe = _ => IO.raiseError(failure),
        show = (_, state) => states.update(_ :+ state),
        update = (_, state) =>
          state match
            case PreviewViewState.Failed(_) =>
              failureEntered.complete(()).void *>
                continueFailure.get *>
                states.update(_ :+ state)
            case _ => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            start <- service.start(workspace).attempt.start
            _     <- failureEntered.get
            stop  <- service.stop(workspace).start
            _     <- continueFailure.complete(())
            _     <- stop.joinWithNever
            _     <- start.joinWithNever
          yield ()
      actual <- states.get
    yield assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("stop racing an unexpected exit converges the panel to stopped"):
    val exit = ProcessExit(Some(1), None, None)
    for
      exited          <- Deferred[IO, ProcessExit]
      exitViewEntered <- Deferred[IO, Unit]
      continueExit    <- Deferred[IO, Unit]
      states          <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(exited.get)),
        probe = _ => IO.pure(true),
        show = (_, state) => states.update(_ :+ state),
        update = (_, state) =>
          state match
            case PreviewViewState.Exited(_) =>
              exitViewEntered.complete(()).void *>
                continueExit.get *>
                states.update(_ :+ state)
            case _ => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _    <- service.start(workspace)
            _    <- exited.complete(exit)
            _    <- exitViewEntered.get
            stop <- service.stop(workspace).start
            _    <- continueExit.complete(())
            _    <- stop.joinWithNever
          yield ()
      actual <- states.get
    yield assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("an old terminal view completes before a replacement session starts"):
    val exit = ProcessExit(Some(1), None, None)
    for
      launches        <- Ref.of[IO, Int](0)
      oldExited       <- Deferred[IO, ProcessExit]
      exitViewEntered <- Deferred[IO, Unit]
      continueExit    <- Deferred[IO, Unit]
      ordering        <- Ref.of[IO, Vector[String]](Vector.empty)
      states          <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.eval:
            launches
              .modify(count => (count + 1) -> count)
              .map:
                case 0 => RunningProcess(oldExited.get)
                case _ => RunningProcess(IO.never),
        probe = _ => IO.pure(true),
        show = (_, state) => states.update(_ :+ state),
        update = (_, state) =>
          state match
            case PreviewViewState.Exited(_) =>
              exitViewEntered.complete(()).void *>
                continueExit.get *>
                ordering.update(_ :+ "old-exited") *>
                states.update(_ :+ state)
            case PreviewViewState.Starting =>
              launches.get.flatMap: count =>
                IO.whenA(count > 1)(ordering.update(_ :+ "new-start")) *>
                  states.update(_ :+ state)
            case _ => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      actualStates <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _       <- service.start(workspace)
            _       <- oldExited.complete(exit)
            _       <- exitViewEntered.get
            restart <- service.restart(workspace).start
            _       <- IO.cede
            _       <- continueExit.complete(())
            _       <- restart.joinWithNever
            current <- states.get
          yield current
      actualOrdering <- ordering.get
    yield
      assertEquals(actualOrdering, Vector("old-exited", "new-start"))
      assertEquals(
        actualStates.lastOption,
        Some(PreviewViewState.Ready(urlOf(command))),
      )

  test("stop suppresses an await-exit observation failure after readiness"):
    val failure = IllegalStateException("observation failed during stop")
    for
      failExit <- Deferred[IO, Unit]
      states   <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(
            IO.pure(RunningProcess(failExit.get *> IO.raiseError(failure))),
          )(_ => failExit.complete(()).void),
        probe = _ => IO.pure(true),
        show = (_, state) => states.update(_ :+ state),
        update = (_, state) => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *> service.stop(workspace)
      actual <- states.get
    yield
      assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))
      assert(!actual.exists:
        case PreviewViewState.Failed(_) => true
        case _                          => false)

  test("propagates an await-exit failure during startup"):
    val failure = IllegalStateException("child process observation failed")
    val algebra = PreviewAlgebra[IO](
      load = _ => IO.pure(command),
      policy = _ => IO.pure(policy),
      launch = (_, _) => Resource.pure(RunningProcess(IO.raiseError(failure))),
      probe = _ => IO.never,
      show = (_, _) => IO.unit,
      update = (_, _) => IO.unit,
      openExternal = _ => IO.unit,
      info = _ => IO.unit,
      error = _ => IO.unit,
    )
    PreviewService
      .resource(algebra)
      .use(_.start(workspace).attempt)
      .map(assertEquals(_, Left(failure)))

  test("terminal cleanup does not block another workspace"):
    val second = workspace.copy(
      key = "file:///workspace-two",
      name = "example-two",
      root = "/workspace-two",
      uri = "file:///workspace-two",
    )
    val failure       = IllegalStateException("startup failed")
    val secondCommand = command.copy(endpoint =
      PreviewEndpoint.Fixed(
        right(PreviewUrl.parse("http://127.0.0.1:5252")),
      ),
    )
    for
      closeEntered   <- Deferred[IO, Unit]
      continueClose  <- Deferred[IO, Unit]
      secondLaunched <- Deferred[IO, Unit]
      algebra = PreviewAlgebra[IO](
        load = current =>
          IO.pure(if current.key == workspace.key then command
          else secondCommand),
        policy = _ => IO.pure(policy),
        launch = (current, _) =>
          if current.key == workspace.key then
            Resource.make(IO.pure(RunningProcess(IO.never)))(_ =>
              closeEntered.complete(()).void *> continueClose.get,
            )
          else
            Resource.eval(
              secondLaunched.complete(()).as(RunningProcess(IO.never)),
            )
        ,
        probe = url =>
          if url == urlOf(command) then IO.raiseError(failure)
          else IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            firstStart  <- service.start(workspace).attempt.start
            _           <- closeEntered.get
            secondStart <- service.start(second).start
            _           <- secondLaunched.get
            _           <- continueClose.complete(())
            _           <- firstStart.joinWithNever
            _           <- secondStart.joinWithNever
          yield ()
    yield ()

  test("attempts every release and retries a failed session cleanup"):
    val second = workspace.copy(
      key = "file:///workspace-two",
      name = "example-two",
      root = "/workspace-two",
      uri = "file:///workspace-two",
    )
    for
      releases <- Ref.of[IO, Int](0)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (current, _) =>
          Resource.make(IO.pure(RunningProcess(IO.never)))(_ =>
            releases.update(_ + 1) *>
              IO.raiseWhen(current.key == workspace.key)(
                IllegalStateException("release failed"),
              ),
          ),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          service.start(workspace) *>
            service.start(second) *>
            service.stopAll
      count <- releases.get
    yield assertEquals(count, 4)

  test("stops independent workspace sessions concurrently"):
    val second = workspace.copy(
      key = "file:///workspace-two",
      name = "example-two",
      root = "/workspace-two",
      uri = "file:///workspace-two",
    )
    for
      firstEntered  <- Deferred[IO, Unit]
      secondEntered <- Deferred[IO, Unit]
      continueClose <- Deferred[IO, Unit]
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(command),
        policy = _ => IO.pure(policy),
        launch = (current, _) =>
          Resource.make(IO.pure(RunningProcess(IO.never)))(_ =>
            (if current.key == workspace.key then firstEntered
             else secondEntered).complete(()).void *>
              continueClose.get,
          ),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          for
            _        <- service.start(workspace)
            _        <- service.start(second)
            stopping <- service.stopAll.start
            _        <- (firstEntered.get, secondEntered.get).parTupled
              .timeout(1.second)
              .guarantee(continueClose.complete(()).void)
            _ <- stopping.joinWithNever
          yield ()
    yield ()

  test("announced IPC URL is probed, stored, and opened as the ready URL"):
    val dynamic   = command.copy(endpoint = PreviewEndpoint.Announced)
    val announced = right(PreviewUrl.parse("http://127.0.0.1:53111"))
    for
      probes <- Ref.of[IO, Vector[PreviewUrl]](Vector.empty)
      states <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      opened <- Ref.of[IO, Vector[PreviewUrl]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(dynamic),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.pure(
            RunningProcess.announced(
              awaitExit = IO.never,
              awaitUrl = IO.pure(announced),
            ),
          ),
        probe = url => probes.update(_ :+ url).as(true),
        show = (_, state) => states.update(_ :+ state),
        update = (_, state) => states.update(_ :+ state),
        openExternal = url => opened.update(_ :+ url),
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      actualStates <- PreviewService
        .resource(algebra)
        .use(service =>
          service.start(workspace) *>
            service.open(workspace) *>
            service.openExternal(workspace) *>
            states.get,
        )
      actualProbes <- probes.get
      actualOpened <- opened.get
    yield
      assertEquals(actualProbes, Vector(announced))
      assertEquals(actualOpened, Vector(announced))
      assertEquals(
        actualStates.lastOption,
        Some(PreviewViewState.Ready(announced)),
      )

  test("dynamic startup rejects a process without an announcement channel"):
    val dynamic = command.copy(endpoint = PreviewEndpoint.Announced)
    for
      releases <- Ref.of[IO, Int](0)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(dynamic),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.make(IO.pure(RunningProcess[IO](IO.never)))(_ =>
            releases.update(_ + 1),
          ),
        probe = _ => IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      result <- PreviewService.resource(algebra).use(_.start(workspace).attempt)
      released <- releases.get
    yield
      result match
        case Left(DynamicPreviewAnnouncementUnavailable(actual)) =>
          assertEquals(actual, workspace.name)
        case other =>
          fail(s"expected DynamicPreviewAnnouncementUnavailable, got $other")
      assertEquals(released, 1)

  test("dynamic startup reports process exit before ready announcement"):
    val dynamic = command.copy(endpoint = PreviewEndpoint.Announced)
    val exit    = ProcessExit(code = Some(2), signal = None, error = None)
    val algebra = PreviewAlgebra[IO](
      load = _ => IO.pure(dynamic),
      policy = _ => IO.pure(policy),
      launch = (_, _) =>
        Resource.pure(
          RunningProcess.announced(
            awaitExit = IO.pure(exit),
            awaitUrl = IO.never,
          ),
        ),
      probe = _ => IO.pure(false),
      show = (_, _) => IO.unit,
      update = (_, _) => IO.unit,
      openExternal = _ => IO.unit,
      info = _ => IO.unit,
      error = _ => IO.unit,
    )
    PreviewService
      .resource(algebra)
      .use(_.start(workspace).attempt)
      .map: result =>
        assert(result.swap.exists:
          case PreviewExited(_, actual) => actual == exit
          case _                        => false)

  test(
    "dynamic startup reports process exit after ready but before HTTP probe",
  ):
    val dynamic   = command.copy(endpoint = PreviewEndpoint.Announced)
    val announced = right(PreviewUrl.parse("http://127.0.0.1:53112"))
    val exit      = ProcessExit(code = Some(3), signal = None, error = None)
    val algebra   = PreviewAlgebra[IO](
      load = _ => IO.pure(dynamic),
      policy = _ => IO.pure(policy),
      launch = (_, _) =>
        Resource.pure(
          RunningProcess.announced(
            awaitExit = IO.pure(exit),
            awaitUrl = IO.pure(announced),
          ),
        ),
      probe = _ => IO.never,
      show = (_, _) => IO.unit,
      update = (_, _) => IO.unit,
      openExternal = _ => IO.unit,
      info = _ => IO.unit,
      error = _ => IO.unit,
    )
    PreviewService
      .resource(algebra)
      .use(_.start(workspace).attempt)
      .map: result =>
        assert(result.swap.exists:
          case PreviewExited(_, actual) => actual == exit
          case _                        => false)

  test("dynamic startup timeout includes announcement and HTTP probe"):
    val timeout = right(StartupTimeout.parse(25))
    val dynamic = command.copy(
      endpoint = PreviewEndpoint.Announced,
      startupTimeout = timeout,
    )
    val algebra = PreviewAlgebra[IO](
      load = _ => IO.pure(dynamic),
      policy = _ => IO.pure(policy),
      launch = (_, _) =>
        Resource.pure(
          RunningProcess.announced(
            awaitExit = IO.never,
            awaitUrl = IO.never,
          ),
        ),
      probe = _ => IO.pure(false),
      show = (_, _) => IO.unit,
      update = (_, _) => IO.unit,
      openExternal = _ => IO.unit,
      info = _ => IO.unit,
      error = _ => IO.unit,
    )
    PreviewService
      .resource(algebra)
      .use(_.start(workspace).attempt)
      .map: result =>
        assert(result.swap.exists:
          case PreviewTimedOut(_, actual) => actual == timeout
          case _                          => false)

  test("restart replaces the announced URL instead of reusing the old URL"):
    val dynamic = command.copy(endpoint = PreviewEndpoint.Announced)
    val first   = right(PreviewUrl.parse("http://127.0.0.1:53113"))
    val second  = right(PreviewUrl.parse("http://127.0.0.1:53114"))
    for
      launches <- Ref.of[IO, Int](0)
      states   <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(dynamic),
        policy = _ => IO.pure(policy),
        launch = (_, _) =>
          Resource.eval(
            launches.modify: count =>
              val url = if count == 0 then first else second
              count + 1 -> RunningProcess.announced(
                awaitExit = IO.never,
                awaitUrl = IO.pure(url),
              ),
          ),
        probe = _ => IO.pure(true),
        show = (_, state) => states.update(_ :+ state),
        update = (_, state) => states.update(_ :+ state),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      actual <- PreviewService
        .resource(algebra)
        .use(service =>
          service.start(workspace) *> service.restart(workspace) *> states.get,
        )
    yield
      assert(actual.contains(PreviewViewState.Ready(first)))
      assertEquals(actual.lastOption, Some(PreviewViewState.Ready(second)))

  test("two dynamic workspaces keep independently announced URLs"):
    val dynamic         = command.copy(endpoint = PreviewEndpoint.Announced)
    val secondWorkspace = workspace.copy(
      key = "file:///workspace-two",
      name = "example-two",
      root = "/workspace-two",
      uri = "file:///workspace-two",
    )
    val firstUrl  = right(PreviewUrl.parse("http://127.0.0.1:53115"))
    val secondUrl = right(PreviewUrl.parse("http://127.0.0.1:53116"))
    for
      states <- Ref.of[IO, Vector[(Workspace, PreviewViewState)]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(dynamic),
        policy = _ => IO.pure(policy),
        launch = (current, _) =>
          Resource.pure(
            RunningProcess.announced(
              awaitExit = IO.never,
              awaitUrl = IO.pure(
                if current.key == workspace.key then firstUrl else secondUrl,
              ),
            ),
          ),
        probe = _ => IO.pure(true),
        show = (current, state) => states.update(_ :+ (current -> state)),
        update = (current, state) => states.update(_ :+ (current -> state)),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(algebra)
        .use: service =>
          (
            service.start(workspace),
            service.start(secondWorkspace),
          ).parTupled.void
      actual <- states.get
    yield
      assert(actual.contains(workspace -> PreviewViewState.Ready(firstUrl)))
      assert(
        actual.contains(secondWorkspace -> PreviewViewState.Ready(secondUrl)),
      )

  test(
    "open commands explain that an inactive dynamic preview must be started",
  ):
    val dynamic = command.copy(endpoint = PreviewEndpoint.Announced)
    for
      states   <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      messages <- Ref.of[IO, Vector[String]](Vector.empty)
      algebra = PreviewAlgebra[IO](
        load = _ => IO.pure(dynamic),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(IO.never)),
        probe = _ => IO.pure(false),
        show = (_, state) => states.update(_ :+ state),
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = message => messages.update(_ :+ message),
        error = _ => IO.unit,
      )
      external <- PreviewService
        .resource(algebra)
        .use: service =>
          service.open(workspace) *> service.openExternal(workspace).attempt
      actualStates   <- states.get
      actualMessages <- messages.get
    yield
      assert(actualStates.exists:
        case PreviewViewState.Failed(reason) => reason.contains("Start")
        case _                               => false)
      assert(actualMessages.exists(_.contains("Start")))
      external match
        case Left(DynamicPreviewNotStarted(actual)) =>
          assertEquals(actual, workspace.name)
        case other => fail(s"expected DynamicPreviewNotStarted, got $other")

  private def healthyAlgebra(
      launches: Ref[IO, Int],
      releases: Ref[IO, Int],
  ): PreviewAlgebra[IO] =
    PreviewAlgebra[IO](
      load = _ => IO.pure(command),
      policy = _ => IO.pure(policy),
      launch = (_, _) =>
        Resource.make(launches.update(_ + 1).as(RunningProcess(IO.never)))(_ =>
          releases.update(_ + 1),
        ),
      probe = _ => IO.pure(true),
      show = (_, _) => IO.unit,
      update = (_, _) => IO.unit,
      openExternal = _ => IO.unit,
      info = _ => IO.unit,
      error = _ => IO.unit,
    )

  private def right[A](value: Either[ConfigProblem, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private def urlOf(value: PreviewCommand): PreviewUrl =
    value.endpoint match
      case PreviewEndpoint.Fixed(url) => url
      case PreviewEndpoint.Announced  => fail("expected a fixed preview URL")

  private def assertStoppedDuringStartup(
      result: Either[Throwable, Unit],
  ): Unit =
    assert(result.swap.exists:
      case PreviewStoppedDuringStartup(_) => true
      case _                              => false)
