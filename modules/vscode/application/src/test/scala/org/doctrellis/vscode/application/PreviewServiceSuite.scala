package org.doctrellis.vscode.application

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
      right(PreviewUrl.parse("http://127.0.0.1:4242")),
      right(StartupTimeout.parse(90000)),
    )

  private val policy = PreviewPolicy(false, right(PollInterval.parse(500)))

  test("starts at most one process for a workspace"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      ports = healthyPorts(launches, releases)
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          service.start(workspace) *> service.start(workspace) *> launches.get
            .map(assertEquals(_, 1))
      released <- releases.get
    yield assertEquals(released, 1)

  test("serializes concurrent starts for the same workspace"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      ports = healthyPorts(launches, releases)
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          List.fill(8)(service.start(workspace)).parSequence_ *> launches.get
            .map(assertEquals(_, 1))
      released <- releases.get
    yield assertEquals(released, 1)

  test("reports an exit that happens before readiness"):
    val exit  = ProcessExit(Some(1), None, None)
    val ports = PreviewPorts[IO](
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
      .resource(ports)
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
      ports = healthyPorts(launches, releases)
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          service.start(workspace) *> service.stop(workspace)
      released <- releases.get
    yield assertEquals(released, 1)

  test("explicit stop leaves an existing preview view in the stopped state"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      views    <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      base  = healthyPorts(launches, releases)
      ports = base.copy(update = (_, state) => views.update(_ :+ state))
      _ <- PreviewService
        .resource(ports)
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
      ports = PreviewPorts[IO](
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
        .resource(ports)
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
      ports = PreviewPorts[IO](
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
        .resource(ports)
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
        Some(PreviewViewState.Ready(command.url)),
      )

  test("open passes the workspace and session URL to the preview view"):
    for
      shown <- Ref.of[IO, Vector[(Workspace, PreviewViewState)]](Vector.empty)
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      base  = healthyPorts(launches, releases)
      ports = base.copy(show =
        (current, state) => shown.update(_ :+ (current -> state)),
      )
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          service.start(workspace) *> service.open(workspace)
      actual <- shown.get
    yield assertEquals(
      actual.lastOption,
      Some(workspace -> PreviewViewState.Ready(command.url)),
    )

  test("openExternal uses the active session URL or loads the configured URL"):
    val configured = command.copy(
      url = right(PreviewUrl.parse("http://127.0.0.1:5252")),
    )
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      loads    <- Ref.of[IO, Int](0)
      opened   <- Ref.of[IO, Vector[PreviewUrl]](Vector.empty)
      base  = healthyPorts(launches, releases)
      ports = base.copy(
        load = _ =>
          loads.modify: count =>
            (count + 1) -> (if count == 0 then command else configured),
        openExternal = url => opened.update(_ :+ url),
      )
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          service.start(workspace) *>
            service.openExternal(workspace) *>
            service.stop(workspace) *>
            service.openExternal(workspace)
      actual <- opened.get
    yield assertEquals(actual, Vector(command.url, configured.url))

  test("open without an active session reports a stopped preview"):
    for
      shown  <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      probes <- Ref.of[IO, Int](0)
      ports = PreviewPorts[IO](
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
      _      <- PreviewService.resource(ports).use(_.open(workspace))
      count  <- probes.get
      states <- shown.get
    yield
      assertEquals(count, 1)
      assertEquals(states, Vector(PreviewViewState.Stopped))

  test("open shows starting while an active session awaits readiness"):
    for
      launched <- Deferred[IO, Unit]
      shown    <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      ports = PreviewPorts[IO](
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
        .resource(ports)
        .use: service =>
          for
            start <- service.start(workspace).start
            _     <- launched.get
            _     <- service.open(workspace)
            _     <- service.stop(workspace)
            _     <- start.joinWithNever
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
      ports = PreviewPorts[IO](
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
        .resource(ports)
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
      base  = healthyPorts(launches, releases)
      ports = base.copy(
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
        .resource(ports)
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
      ports = PreviewPorts[IO](
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
        .resource(ports)
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
    val replacement = command.copy(
      url = right(PreviewUrl.parse("http://127.0.0.1:5252")),
    )
    for
      loads            <- Ref.of[IO, Int](0)
      showEntered      <- Deferred[IO, Unit]
      continueShow     <- Deferred[IO, Unit]
      replacementReady <- Deferred[IO, Unit]
      states           <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      ports = PreviewPorts[IO](
        load = _ =>
          loads.modify: count =>
            (count + 1) -> (if count == 0 then command else replacement),
        policy = _ => IO.pure(policy),
        launch = (_, _) => Resource.pure(RunningProcess(IO.never)),
        probe = _ => IO.pure(true),
        show = (_, state) =>
          states.update(_ :+ state) *>
            (state match
              case PreviewViewState.Ready(url) if url == command.url =>
                showEntered.complete(()).void *> continueShow.get
              case _ => IO.unit),
        update = (_, state) =>
          states.update(_ :+ state) *>
            (state match
              case PreviewViewState.Ready(url) if url == replacement.url =>
                replacementReady.complete(()).void
              case _ => IO.unit),
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      actual <- PreviewService
        .resource(ports)
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
      Some(PreviewViewState.Ready(replacement.url)),
    )

  test("open-when-ready shows the ready session in its workspace panel"):
    for
      shown <- Ref.of[IO, Vector[(Workspace, PreviewViewState)]](Vector.empty)
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      base  = healthyPorts(launches, releases)
      ports = base.copy(
        policy = _ => IO.pure(policy.copy(openWhenReady = true)),
        show = (current, state) => shown.update(_ :+ (current -> state)),
      )
      _      <- PreviewService.resource(ports).use(_.start(workspace))
      actual <- shown.get
    yield assertEquals(
      actual.lastOption,
      Some(workspace -> PreviewViewState.Ready(command.url)),
    )

  test("configuration or launch failure updates an existing workspace panel"):
    val failure = IllegalArgumentException("broken config <value>")
    for
      states <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      ports = PreviewPorts[IO](
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
      result <- PreviewService.resource(ports).use(_.start(workspace).attempt)
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
      ports = PreviewPorts[IO](
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
        .resource(ports)
        .use: service =>
          for
            start <- service.start(workspace).start
            _     <- launched.get
            _     <- service.stop(workspace)
            _     <- start.joinWithNever
          yield ()
      count  <- released.get
      actual <- states.get
    yield
      assertEquals(count, 1)
      assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("stop racing a ready display converges the panel to stopped"):
    val exit = ProcessExit(None, Some("SIGTERM"), None)
    for
      infoEntered  <- Deferred[IO, Unit]
      continueInfo <- Deferred[IO, Unit]
      exited       <- Deferred[IO, ProcessExit]
      states       <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      ports = PreviewPorts[IO](
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
        .resource(ports)
        .use: service =>
          for
            start <- service.start(workspace).start
            _     <- infoEntered.get
            _     <- service.stop(workspace)
            _     <- continueInfo.complete(())
            _     <- start.joinWithNever
          yield ()
      actual <- states.get
    yield assertEquals(actual.lastOption, Some(PreviewViewState.Stopped))

  test("a replaced session suppresses its old ready continuation"):
    val replacement = command.copy(
      url = right(PreviewUrl.parse("http://127.0.0.1:5252")),
    )
    for
      loads            <- Ref.of[IO, Int](0)
      infoCalls        <- Ref.of[IO, Int](0)
      infoEntered      <- Deferred[IO, Unit]
      continueInfo     <- Deferred[IO, Unit]
      replacementReady <- Deferred[IO, Unit]
      states           <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      ports = PreviewPorts[IO](
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
              case PreviewViewState.Ready(url) if url == replacement.url =>
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
        .resource(ports)
        .use: service =>
          for
            firstStart <- service.start(workspace).start
            _          <- infoEntered.get
            restart    <- service.restart(workspace).start
            _          <- replacementReady.get
            _          <- continueInfo.complete(())
            _          <- firstStart.joinWithNever
            _          <- restart.joinWithNever
            current    <- states.get
          yield current
    yield assertEquals(
      actual.lastOption,
      Some(PreviewViewState.Ready(replacement.url)),
    )

  test("stop racing a startup failure converges the panel to stopped"):
    val failure = IllegalStateException("startup probe failed")
    for
      failureEntered  <- Deferred[IO, Unit]
      continueFailure <- Deferred[IO, Unit]
      states          <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      ports = PreviewPorts[IO](
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
        .resource(ports)
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
      ports = PreviewPorts[IO](
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
        .resource(ports)
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
      ports = PreviewPorts[IO](
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
        .resource(ports)
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
        Some(PreviewViewState.Ready(command.url)),
      )

  test("stop suppresses an await-exit observation failure after readiness"):
    val failure = IllegalStateException("observation failed during stop")
    for
      failExit <- Deferred[IO, Unit]
      states   <- Ref.of[IO, Vector[PreviewViewState]](Vector.empty)
      ports = PreviewPorts[IO](
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
        .resource(ports)
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
    val ports   = PreviewPorts[IO](
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
      .resource(ports)
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
    val secondCommand = command.copy(
      url = right(PreviewUrl.parse("http://127.0.0.1:5252")),
    )
    for
      closeEntered   <- Deferred[IO, Unit]
      continueClose  <- Deferred[IO, Unit]
      secondLaunched <- Deferred[IO, Unit]
      ports = PreviewPorts[IO](
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
          if url == command.url then IO.raiseError(failure)
          else IO.pure(true),
        show = (_, _) => IO.unit,
        update = (_, _) => IO.unit,
        openExternal = _ => IO.unit,
        info = _ => IO.unit,
        error = _ => IO.unit,
      )
      _ <- PreviewService
        .resource(ports)
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

  test("attempts every release when one session cleanup fails"):
    val second = workspace.copy(
      key = "file:///workspace-two",
      name = "example-two",
      root = "/workspace-two",
      uri = "file:///workspace-two",
    )
    for
      releases <- Ref.of[IO, Int](0)
      ports = PreviewPorts[IO](
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
        .resource(ports)
        .use: service =>
          service.start(workspace) *>
            service.start(second) *>
            service.stopAll
      count <- releases.get
    yield assertEquals(count, 2)

  private def healthyPorts(
      launches: Ref[IO, Int],
      releases: Ref[IO, Int],
  ): PreviewPorts[IO] =
    PreviewPorts[IO](
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
