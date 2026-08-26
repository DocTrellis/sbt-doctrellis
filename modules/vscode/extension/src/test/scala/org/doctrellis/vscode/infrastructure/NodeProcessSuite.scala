package org.doctrellis.vscode.infrastructure

import scala.concurrent.duration.*
import scala.scalajs.js
import scala.util.matching.Regex

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.effect.std.Dispatcher
import cats.syntax.all.*

import munit.CatsEffectSuite

import org.doctrellis.vscode.application.RunningProcess
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.{
  NodeFsPromises,
  NodeOs,
  NodePath,
  NodeRuntime,
  TestNodeFsPromises,
  TestNodeRuntime,
}

class NodeProcessSuite extends CatsEffectSuite:
  private val nodePathSemantics =
    new PathSemantics(NodePath.sep, NodePath.isAbsolute)

  private val currentPlatform =
    ProcessPlatform
      .parse(NodeRuntime.platform)
      .fold(problem => fail(problem.getMessage), identity)

  private def reportedTermination(exit: ProcessExit): Boolean = exit match
    case ProcessExit.Terminated(_)          => true
    case ProcessExit.Failed(_, termination) => termination.nonEmpty
    case ProcessExit.Unknown                => false

  private val workspace = Workspace(
    key = right(WorkspaceId.parse("file:///node-process-suite")),
    name = "node-process-suite",
    root = TestNodeRuntime.cwd(),
  )

  private val exampleWorkspace = Workspace(
    key = right(WorkspaceId.parse("file:///doctrellis-basic-example")),
    name = "doctrellis-basic-example",
    root = NodePath.join(TestNodeRuntime.cwd(), "examples", "basic"),
  )

  test("stops a ready process, releases its port, and tolerates repeat stop"):
    Dispatcher.parallel[IO](await = false).use(testReadyStop)

  test("forces only its TERM-ignoring POSIX process group"):
    if currentPlatform == ProcessPlatform.Windows
    then IO.unit
    else Dispatcher.parallel[IO](await = false).use(testForcedTree)

  test("selects the exact Windows task tree command"):
    IO:
      assertEquals(
        ProcessPlatform.parse("win32"),
        Right(ProcessPlatform.Windows),
      )
      assertEquals(
        ProcessPlatform.parse("linux"),
        Right(ProcessPlatform.Posix),
      )
      assertEquals(
        ProcessPlatform.parse("android"),
        Right(ProcessPlatform.Posix),
      )
      assertEquals(
        ProcessPlatform.parse("netbsd"),
        Right(ProcessPlatform.Posix),
      )
      assert(ProcessPlatform.parse("unknown").isLeft)
      assertEquals(
        OwnedProcess.taskkillArguments(pid = 1234, force = false),
        Vector("/PID", "1234", "/T"),
      )

  test("process errors use the Node code instead of message substrings"):
    IO:
      val missing = new js.JavaScriptException(
        js.Dynamic.literal(code = "ESRCH", message = "process unavailable"),
      )
      val misleading = new js.JavaScriptException(
        js.Dynamic.literal(code = "EIO", message = "path contains EPERM"),
      )
      assert(NodeError.hasCode(missing, "ESRCH"))
      assert(!NodeError.hasCode(misleading, "EPERM"))
      assertEquals(
        OwnedProcess.taskkillArguments(pid = 1234, force = true),
        Vector("/PID", "1234", "/T", "/F"),
      )
      assertEquals(
        OwnedProcess.taskkillExecutable(Some("C:\\Windows")),
        "C:\\Windows\\System32\\taskkill.exe",
      )
      assertEquals(
        OwnedProcess.taskkillExecutable(None),
        "taskkill.exe",
      )

  test("a graceful signal failure uses the force fallback"):
    for
      forced   <- Ref.of[IO, Int](0)
      observed <- Ref.of[IO, Option[String]](None)
      _        <- ProcessShutdown.gracefulThenForce(
        graceful = IO.raiseError(IllegalStateException("graceful failed")),
        awaitGraceful = IO.unit,
        force = forced.update(_ + 1),
        awaitForced = IO.unit,
        onForce = error => observed.set(Option(error.getMessage)),
      )
      actual  <- forced.get
      message <- observed.get
    yield
      assertEquals(actual, 1)
      assertEquals(message, Some("graceful failed"))

  test("a process helper is bounded"):
    ProcessShutdown
      .bounded(
        label = "fixture helper",
        timeout = 20.millis,
        action = IO.never,
      )
      .attempt
      .map: result =>
        assert(
          result.swap.exists(error =>
            Option(error.getMessage).contains(
              "fixture helper timed out after 20 ms.",
            ),
          ),
        )

  test("a confirmed POSIX group exit tolerates a missing close event"):
    for
      closed <- Deferred[IO, ProcessExit]
      logs   <- Ref.of[IO, String]("")
      _      <- ProcessClose.confirm(
        closed = closed,
        timeout = 20.millis,
        synthesize = true,
        append = chunk => logs.update(_ + chunk),
        workspaceName = "fixture",
        pid = 1234,
      )
      exit <- closed.get
      log  <- logs.get
    yield
      assertEquals(
        exit,
        ProcessExit.Failed(
          "close notification was not delivered",
          termination = None,
        ),
      )
      assert(log.contains("confirmed group exit"))

  test("a missing Windows close event remains a shutdown failure"):
    for
      closed <- Deferred[IO, ProcessExit]
      result <- ProcessClose
        .confirm(
          closed = closed,
          timeout = 20.millis,
          synthesize = false,
          append = _ => IO.unit,
          workspaceName = "fixture",
          pid = 1234,
        )
        .attempt
    yield assert(result.isLeft)

  test("a spawn failure logs its operating-system cause"):
    Dispatcher
      .parallel[IO](await = false)
      .use: dispatcher =>
        for
          logs     <- Ref.of[IO, String]("")
          reported <- Deferred[IO, Unit]
          append = (chunk: String) =>
            logs.update(_ + chunk) *>
              IO.whenA(chunk.contains("spawn failed"))(
                reported.complete(()).void,
              )
          result <- NodeProcess
            .launch(dispatcher, append)(workspace, missingCommand)
            .use(_ => IO.unit)
            .attempt
          _      <- reported.get.timeout(5.seconds)
          actual <- logs.get
        yield
          assert(result.isLeft)
          assert(actual.contains("ENOENT"))

  test("reports metadata version skew without launching a replacement"):
    Dispatcher.parallel[IO](await = false).use(testMetadataVersionSkew)

  test("stops the actual basic sbt preview without leaving its listener"):
    if !runExampleIntegration then IO.unit
    else Dispatcher.parallel[IO](await = false).use(testExampleLifecycle)

  test("runs two actual workspace previews on distinct dynamic ports"):
    if !runExampleIntegration then IO.unit
    else Dispatcher.parallel[IO](await = false).use(testTwoExampleWorkspaces)

  test("signal fallback cleans the actual workspace IPC endpoint"):
    if !runExampleIntegration then IO.unit
    else Dispatcher.parallel[IO](await = false).use(testSignalEndpointCleanup)

  test("replaces stale metadata whose pid and port belong to another process"):
    if !runExampleIntegration then IO.unit
    else Dispatcher.parallel[IO](await = false).use(testRecycledTcpMetadata)

  test("replaces malformed workspace metadata"):
    if !runExampleIntegration then IO.unit
    else Dispatcher.parallel[IO](await = false).use(testMalformedMetadata)

  private def testReadyStop(dispatcher: Dispatcher[IO]): IO[Unit] =
    for
      firstReady <- Deferred[IO, Started]
      firstLog   <- Ref.of[IO, String]("")
      first = command(serverScript(port = 0, ignoreTerm = false))
      result <- managedProcess(
        dispatcher = dispatcher,
        command = first,
        append = capture(firstLog, firstReady),
        timeouts = ProcessShutdownTimeouts.default,
      ).use:
        case (running, stop) =>
          for
            started <- firstReady.get.timeout(5.seconds)
            url     <- IO(right(PreviewUrl.parse(started.url)))
            ready   <- NodeHttpProbe.ready(url)
            _       <- phase("first stop", stop)
            exit    <- running.awaitExit
            _       <- phase("repeat stop", stop)
            stopped <- NodeHttpProbe.ready(url)
            rebound <- phase(
              "port rebind",
              rebind(dispatcher, started.port),
            )
            alive <- processIsAlive(started.pid)
          yield (ready, exit, stopped, rebound, alive)
      (ready, exit, stopped, rebound, alive) = result
    yield
      assert(ready)
      if currentPlatform == ProcessPlatform.Posix then
        assertEquals(
          exit,
          ProcessExit.Terminated(ProcessTermination.Signal("SIGTERM")),
        )
      else assert(reportedTermination(exit))
      assert(!stopped)
      assert(rebound)
      assert(!alive)

  private def testMetadataVersionSkew(
      dispatcher: Dispatcher[IO],
  ): IO[Unit] =
    for
      endpoint <- WorkspaceChannel.endpoint(workspace)
      ready    <- Deferred[IO, Started]
      log      <- Ref.of[IO, String]("")
      result   <- managedProcess(
        dispatcher = dispatcher,
        command = command(serverScript(port = 0, ignoreTerm = false)),
        append = capture(log, ready),
        timeouts = ProcessShutdownTimeouts.default,
      ).use:
        case (_, stopFixture) =>
          for
            fixture <- ready.get.timeout(5.seconds)
            incompatible =
              s"""{"version":2,"protocol":"${WorkspaceChannel.Protocol}","transport":"tcp","host":"127.0.0.1","port":${fixture.port},"pid":${fixture.pid},"sessionId":"future-session","launchId":"future-launch","token":"${"0" * 64}"}"""
            _ <- promise(
              TestNodeFsPromises.writeFile(
                endpoint.metadataPath,
                incompatible,
              ),
            )
            launchLog <- Ref.of[IO, String]("")
            attempted <- WorkspacePreview
              .launch(dispatcher, chunk => launchLog.update(_ + chunk))(
                workspace,
                missingCommand.copy(endpoint = PreviewEndpoint.Announced),
              )
              .use(_ => IO.unit)
              .attempt
            output <- launchLog.get
            served <- NodeHttpProbe.ready(
              right(PreviewUrl.parse(fixture.url)),
            )
            _ <- promise(
              TestNodeFsPromises.rm(
                endpoint.metadataPath,
                js.Dynamic.literal(force = true),
              ),
            )
            _ <- stopFixture
          yield (attempted, output, served)
      (attempted, output, served) = result
    yield
      assert(
        attempted.swap.exists(error =>
          Option(error.getMessage).exists(
            _.contains("unsupported IPC metadata version 2"),
          ),
        ),
      )
      assert(!output.contains("spawn failed"))
      assert(served)

  private def testForcedTree(dispatcher: Dispatcher[IO]): IO[Unit] =
    for
      ownedReady     <- Deferred[IO, Started]
      ownedRoot      <- Deferred[IO, Int]
      ownedLog       <- Ref.of[IO, String]("")
      unrelatedReady <- Deferred[IO, Started]
      unrelatedLog   <- Ref.of[IO, String]("")
      timeouts = ProcessShutdownTimeouts(
        graceful = 200.millis,
        forced = 2.seconds,
        pollInterval = 20.millis,
      )
      processes = managedProcess(
        dispatcher = dispatcher,
        command = command(ignoringTreeScript),
        append = captureTree(ownedLog, ownedRoot, ownedReady),
        timeouts = timeouts,
      ).flatMap: owned =>
        managedProcess(
          dispatcher = dispatcher,
          command = command(serverScript(port = 0, ignoreTerm = false)),
          append = capture(unrelatedLog, unrelatedReady),
          timeouts = timeouts,
        ).map(unrelated => owned -> unrelated)
      result <- processes.use:
        case ((owned, stopOwned), (_, _)) =>
          for
            root         <- ownedRoot.get.timeout(5.seconds)
            child        <- ownedReady.get.timeout(5.seconds)
            other        <- unrelatedReady.get.timeout(5.seconds)
            childUrl     <- IO(right(PreviewUrl.parse(child.url)))
            unrelatedUrl <- IO(right(PreviewUrl.parse(other.url)))
            _            <- phase("forced tree stop", stopOwned)
            exit         <- owned.awaitExit
            childReady   <- NodeHttpProbe.ready(childUrl)
            otherReady   <- NodeHttpProbe.ready(unrelatedUrl)
            rootAlive    <- processIsAlive(root)
            childAlive   <- processIsAlive(child.pid)
            logs         <- ownedLog.get
          yield (
            exit,
            childReady,
            otherReady,
            rootAlive,
            childAlive,
            logs,
          )
      (exit, childReady, otherReady, rootAlive, childAlive, logs) = result
    yield
      assertEquals(
        exit,
        ProcessExit.Terminated(ProcessTermination.Signal("SIGKILL")),
      )
      assert(!childReady)
      assert(otherReady)
      assert(!rootAlive)
      assert(!childAlive)
      assert(logs.contains("forcing its owned process tree"))

  private def testExampleLifecycle(dispatcher: Dispatcher[IO]): IO[Unit] =
    for
      occupiedReady <- Deferred[IO, Started]
      occupiedLog   <- Ref.of[IO, String]("")
      result        <- managedProcess(
        dispatcher = dispatcher,
        command = command(serverScript(port = 4242, ignoreTerm = false)),
        append = capture(occupiedLog, occupiedReady),
        timeouts = ProcessShutdownTimeouts.default,
      ).use:
        case (_, stopOccupier) =>
          for
            occupied    <- occupiedReady.get.timeout(5.seconds)
            ownedPid    <- Deferred[IO, Int]
            log         <- Ref.of[IO, String]("")
            observerLog <- Ref.of[IO, String]("")
            active      <- WorkspacePreview
              .launch(dispatcher, captureOwnedPid(log, ownedPid))(
                exampleWorkspace,
                exampleCommand,
              )
              .use: running =>
                for
                  announced <- IO.fromOption(running.announcedUrl)(
                    IllegalStateException("dynamic URL was not available"),
                  )
                  url            <- announced.timeout(120.seconds)
                  pid            <- ownedPid.get.timeout(10.seconds)
                  served         <- NodeHttpProbe.ready(url)
                  occupiedDuring <- NodeHttpProbe.ready(exampleUrl)
                  observed       <- WorkspacePreview
                    .launch(dispatcher, chunk => observerLog.update(_ + chunk))(
                      exampleWorkspace,
                      exampleCommand,
                    )
                    .use: observer =>
                      IO.fromOption(observer.announcedUrl)(
                        IllegalStateException(
                          "observer dynamic URL was not available",
                        ),
                      ).flatten
                        .timeout(5.seconds)
                  readyAfterObserver <- NodeHttpProbe.ready(url)
                  observerOutput     <- observerLog.get
                yield (
                  running,
                  url,
                  pid,
                  served,
                  occupiedDuring,
                  observed,
                  readyAfterObserver,
                  observerOutput,
                )
            (
              running,
              url,
              pid,
              served,
              occupiedDuring,
              observed,
              readyAfterObserver,
              observerOutput,
            ) = active
            exit          <- running.awaitExit
            stopped       <- NodeHttpProbe.ready(url)
            alive         <- processIsAlive(pid)
            occupiedAfter <- NodeHttpProbe.ready(exampleUrl)
            dynamicPort   <- IO.fromOption(
              url.value.stripPrefix("http://127.0.0.1:").toIntOption,
            )(IllegalStateException(s"unexpected dynamic URL ${url.value}"))
            rebound <- rebind(dispatcher, dynamicPort)
            _       <- stopOccupier
          yield (
            served,
            occupiedDuring,
            occupiedAfter,
            observed,
            readyAfterObserver,
            observerOutput,
            dynamicPort,
            occupied.port,
            exit,
            stopped,
            alive,
            rebound,
          )
      (
        served,
        occupiedDuring,
        occupiedAfter,
        observed,
        readyAfterObserver,
        observerOutput,
        dynamicPort,
        occupiedPort,
        exit,
        stopped,
        alive,
        rebound,
      ) = result
      fixedRegression <- testFixedExampleLifecycle(dispatcher)
    yield
      assert(served)
      assert(occupiedDuring)
      assert(occupiedAfter)
      assertEquals(observed.value, s"http://127.0.0.1:$dynamicPort")
      assert(readyAfterObserver)
      assert(observerOutput.contains("read-only lease"))
      assertEquals(occupiedPort, 4242)
      assertNotEquals(dynamicPort, 4242)
      assert(reportedTermination(exit))
      assert(!stopped)
      assert(!alive)
      assert(rebound)
      assert(fixedRegression)

  private def testFixedExampleLifecycle(
      dispatcher: Dispatcher[IO],
  ): IO[Boolean] =
    for
      ready    <- Deferred[IO, PreviewUrl]
      ownedPid <- Deferred[IO, Int]
      log      <- Ref.of[IO, String]("")
      result   <- managedProcess(
        dispatcher = dispatcher,
        command = exampleFixedCommand,
        append = captureFixedExample(log, ownedPid, ready),
        timeouts = ProcessShutdownTimeouts.default,
      ).use:
        case (running, stop) =>
          for
            url     <- ready.get.timeout(120.seconds)
            pid     <- ownedPid.get.timeout(10.seconds)
            served  <- NodeHttpProbe.ready(url)
            _       <- stop
            _       <- running.awaitExit
            stopped <- NodeHttpProbe.ready(url)
            alive   <- processIsAlive(pid)
          yield served && !stopped && !alive
    yield result

  private def testTwoExampleWorkspaces(
      dispatcher: Dispatcher[IO],
  ): IO[Unit] =
    val source = NodePath.join(TestNodeRuntime.cwd(), "examples", "basic")
    promise(
      TestNodeFsPromises.mkdtemp(
        NodePath.join(NodeOs.tmpdir(), "doctrellis-two-workspaces-"),
      ),
    ).bracket(temporary =>
      val firstRoot  = NodePath.join(temporary, "first")
      val secondRoot = NodePath.join(temporary, "second")
      val first      = Workspace(
        key = right(WorkspaceId.parse(s"file://$firstRoot")),
        name = "first-dynamic-example",
        root = firstRoot,
      )
      val second = Workspace(
        key = right(WorkspaceId.parse(s"file://$secondRoot")),
        name = "second-dynamic-example",
        root = secondRoot,
      )
      for
        _              <- prepareExampleCopy(source, firstRoot)
        _              <- prepareExampleCopy(source, secondRoot)
        firstPid       <- Deferred[IO, Int]
        secondPid      <- Deferred[IO, Int]
        firstLog       <- Ref.of[IO, String]("")
        secondLog      <- Ref.of[IO, String]("")
        firstEndpoint  <- WorkspaceChannel.endpoint(first)
        secondEndpoint <- WorkspaceChannel.endpoint(second)
        active         <- (
          WorkspacePreview.launch(
            dispatcher,
            captureOwnedPid(firstLog, firstPid),
          )(first, dynamicExampleCommand(firstRoot)),
          WorkspacePreview.launch(
            dispatcher,
            captureOwnedPid(secondLog, secondPid),
          )(second, dynamicExampleCommand(secondRoot)),
        ).tupled.use: (firstRunning, secondRunning) =>
          for
            firstAnnouncement <- IO.fromOption(firstRunning.announcedUrl)(
              IllegalStateException("first dynamic URL was not available"),
            )
            secondAnnouncement <- IO.fromOption(secondRunning.announcedUrl)(
              IllegalStateException("second dynamic URL was not available"),
            )
            urls <- (
              firstAnnouncement.timeout(120.seconds),
              secondAnnouncement.timeout(120.seconds),
            ).parTupled
            (firstUrl, secondUrl) = urls
            ready <- (
              NodeHttpProbe.ready(firstUrl),
              NodeHttpProbe.ready(secondUrl),
            ).parTupled
            pids <- (
              firstPid.get.timeout(10.seconds),
              secondPid.get.timeout(10.seconds),
            ).parTupled
          yield (firstRunning, secondRunning, firstUrl, secondUrl, ready, pids)
        (firstRunning, secondRunning, firstUrl, secondUrl, ready, pids) = active
        exits   <- (firstRunning.awaitExit, secondRunning.awaitExit).parTupled
        stopped <- (
          NodeHttpProbe.ready(firstUrl),
          NodeHttpProbe.ready(secondUrl),
        ).parTupled
        alive <- (
          processIsAlive(pids._1),
          processIsAlive(pids._2),
        ).parTupled
        metadataExists <- (
          exists(firstEndpoint.metadataPath),
          exists(secondEndpoint.metadataPath),
        ).parTupled
        ports = (
          dynamicPort(firstUrl),
          dynamicPort(secondUrl),
        )
        rebound <- (
          rebind(dispatcher, ports._1),
          rebind(dispatcher, ports._2),
        ).parTupled
      yield
        assertNotEquals(firstUrl, secondUrl)
        assertNotEquals(ports._1, ports._2)
        assertEquals(ready, (true, true))
        assert(reportedTermination(exits._1))
        assert(reportedTermination(exits._2))
        assertEquals(stopped, (false, false))
        assertEquals(alive, (false, false))
        assertEquals(metadataExists, (false, false))
        assertEquals(rebound, (true, true)),
    )(temporary =>
      promise(
        TestNodeFsPromises.rm(
          temporary,
          js.Dynamic.literal(recursive = true, force = true),
        ),
      ),
    )

  private def testSignalEndpointCleanup(
      dispatcher: Dispatcher[IO],
  ): IO[Unit] =
    for
      endpoint <- WorkspaceChannel.endpoint(exampleWorkspace)
      ready    <- Deferred[IO, PreviewUrl]
      ownedPid <- Deferred[IO, Int]
      log      <- Ref.of[IO, String]("")
      active   <- NodeProcess
        .launchWithEnvironment(
          dispatcher = dispatcher,
          append = captureDynamicExample(log, ownedPid, ready),
          environment = endpoint.environment("signal-cleanup-launch"),
        )(exampleWorkspace, exampleCommand)
        .use: running =>
          for
            url          <- ready.get.timeout(120.seconds)
            pid          <- ownedPid.get.timeout(10.seconds)
            served       <- NodeHttpProbe.ready(url)
            metadataText <- promise(
              NodeFsPromises.readFile(endpoint.metadataPath, "utf8"),
            )
            metadata <- IO.fromEither(
              WorkspaceChannelMetadataParser.parse(metadataText, endpoint),
            )
            socketPath = metadata.transport match
              case WorkspaceChannelTransport.Unix(path) => Some(path)
              case WorkspaceChannelTransport.Tcp(_, _)  => None
            socket <- socketPath.traverse(exists).map(_.exists(identity))
          yield (running, url, pid, served, socketPath, socket)
      (running, url, pid, served, socketPath, socketDuring) = active
      _             <- running.awaitExit
      stopped       <- NodeHttpProbe.ready(url)
      alive         <- processIsAlive(pid)
      metadataAfter <- exists(endpoint.metadataPath)
      socketAfter   <- socketPath.traverse(exists).map(_.exists(identity))
      portReusable  <- rebind(dispatcher, dynamicPort(url))
      expectsUnixFile = currentPlatform == ProcessPlatform.Posix
    yield
      assert(served)
      assertEquals(socketDuring, expectsUnixFile)
      assert(!stopped)
      assert(!alive)
      assert(!metadataAfter)
      assert(!socketAfter)
      assert(portReusable)

  private def testRecycledTcpMetadata(
      dispatcher: Dispatcher[IO],
  ): IO[Unit] =
    for
      endpoint      <- WorkspaceChannel.endpoint(exampleWorkspace)
      occupiedReady <- Deferred[IO, Started]
      occupiedLog   <- Ref.of[IO, String]("")
      result        <- managedProcess(
        dispatcher = dispatcher,
        command = command(serverScript(port = 0, ignoreTerm = false)),
        append = capture(occupiedLog, occupiedReady),
        timeouts = ProcessShutdownTimeouts.default,
      ).use:
        case (_, stopOccupier) =>
          for
            fixture <- occupiedReady.get.timeout(5.seconds)
            stale =
              s"""{"version":1,"protocol":"${WorkspaceChannel.Protocol}","transport":"tcp","host":"127.0.0.1","port":${fixture.port},"pid":${fixture.pid},"sessionId":"stale-session","launchId":"stale-launch","token":"${"0" * 64}"}"""
            _ <- promise(
              TestNodeFsPromises.writeFile(endpoint.metadataPath, stale),
            )
            ownedPid <- Deferred[IO, Int]
            log      <- Ref.of[IO, String]("")
            active   <- WorkspacePreview
              .launch(dispatcher, captureOwnedPid(log, ownedPid))(
                exampleWorkspace,
                exampleCommand,
              )
              .use: running =>
                for
                  announcement <- IO.fromOption(running.announcedUrl)(
                    IllegalStateException("dynamic URL was not available"),
                  )
                  url           <- announcement.timeout(120.seconds)
                  pid           <- ownedPid.get.timeout(10.seconds)
                  served        <- NodeHttpProbe.ready(url)
                  fixtureServed <- NodeHttpProbe.ready(
                    right(PreviewUrl.parse(fixture.url)),
                  )
                yield (running, url, pid, served, fixtureServed)
            (running, url, pid, served, occupiedDuring) = active
            _             <- running.awaitExit
            stopped       <- NodeHttpProbe.ready(url)
            alive         <- processIsAlive(pid)
            metadataAfter <- exists(endpoint.metadataPath)
            _             <- stopOccupier
          yield (served, occupiedDuring, stopped, alive, metadataAfter)
      (served, occupiedDuring, stopped, alive, metadataAfter) = result
    yield
      assert(served)
      assert(occupiedDuring)
      assert(!stopped)
      assert(!alive)
      assert(!metadataAfter)

  private def testMalformedMetadata(
      dispatcher: Dispatcher[IO],
  ): IO[Unit] =
    for
      endpoint <- WorkspaceChannel.endpoint(exampleWorkspace)
      _        <- promise(
        TestNodeFsPromises.writeFile(endpoint.metadataPath, "not-json"),
      )
      ownedPid <- Deferred[IO, Int]
      log      <- Ref.of[IO, String]("")
      active   <- WorkspacePreview
        .launch(dispatcher, captureOwnedPid(log, ownedPid))(
          exampleWorkspace,
          exampleCommand,
        )
        .use: running =>
          for
            announcement <- IO.fromOption(running.announcedUrl)(
              IllegalStateException("dynamic URL was not available"),
            )
            url    <- announcement.timeout(120.seconds)
            pid    <- ownedPid.get.timeout(10.seconds)
            served <- NodeHttpProbe.ready(url)
          yield (running, url, pid, served)
      (running, url, pid, served) = active
      _             <- running.awaitExit
      stopped       <- NodeHttpProbe.ready(url)
      alive         <- processIsAlive(pid)
      metadataAfter <- exists(endpoint.metadataPath)
    yield
      assert(served)
      assert(!stopped)
      assert(!alive)
      assert(!metadataAfter)

  private def prepareExampleCopy(
      source: String,
      destination: String,
  ): IO[Unit] =
    val recursive = js.Dynamic.literal(recursive = true, force = true)
    val parents   = js.Dynamic.literal(recursive = true)
    promise(TestNodeFsPromises.mkdir(destination, parents)).void *>
      promise(
        TestNodeFsPromises.cp(
          NodePath.join(source, "build.sbt"),
          NodePath.join(destination, "build.sbt"),
          js.Dynamic.literal(force = true),
        ),
      ) *>
      promise(
        TestNodeFsPromises.cp(
          NodePath.join(source, "project"),
          NodePath.join(destination, "project"),
          recursive,
        ),
      ) *>
      promise(
        TestNodeFsPromises.cp(
          NodePath.join(source, "docs"),
          NodePath.join(destination, "docs"),
          recursive,
        ),
      )

  private def dynamicExampleCommand(directory: String): PreviewCommand =
    exampleCommand.copy(workingDirectory =
      right(
        WorkingDirectory.parseResolved(
          resolved = directory,
          relativeFromRoot = "",
          paths = nodePathSemantics,
        ),
      ),
    )

  private def dynamicPort(url: PreviewUrl): Int =
    url.value
      .stripPrefix("http://127.0.0.1:")
      .toIntOption
      .getOrElse(fail(s"unexpected dynamic URL ${url.value}"))

  private def exists(path: String): IO[Boolean] =
    promise(TestNodeFsPromises.access(path)).as(true).handleError(_ => false)

  private def promise[A](value: => js.Promise[A]): IO[A] =
    IO.fromPromise(IO.delay(value))

  private def managedProcess(
      dispatcher: Dispatcher[IO],
      command: PreviewCommand,
      append: String => IO[Unit],
      timeouts: ProcessShutdownTimeouts,
  ): Resource[IO, (RunningProcess[IO], IO[Unit])] =
    Resource
      .make(
        NodeProcess
          .launchWithTimeouts(
            dispatcher = dispatcher,
            append = append,
            timeouts = timeouts,
          )(workspace, command)
          .allocated,
      )((_, release) => release.handleError(_ => ()))
      .map:
        case (running, release) => running -> release

  private def rebind(dispatcher: Dispatcher[IO], port: Int): IO[Boolean] =
    for
      ready  <- Deferred[IO, Started]
      log    <- Ref.of[IO, String]("")
      result <- managedProcess(
        dispatcher = dispatcher,
        command = command(serverScript(port, ignoreTerm = false)),
        append = capture(log, ready),
        timeouts = ProcessShutdownTimeouts.default,
      ).use:
        case (_, stop) => ready.get.timeout(5.seconds) *> stop.as(true)
    yield result

  private def capture(
      output: Ref[IO, String],
      ready: Deferred[IO, Started],
  )(chunk: String): IO[Unit] =
    output
      .updateAndGet(_ + chunk)
      .flatMap: current =>
        parseStarted(current).traverse_(value => ready.complete(value).void)

  private def captureTree(
      output: Ref[IO, String],
      root: Deferred[IO, Int],
      ready: Deferred[IO, Started],
  )(chunk: String): IO[Unit] =
    output
      .updateAndGet(_ + chunk)
      .flatMap: current =>
        parseRoot(current).traverse_(value => root.complete(value).void) *>
          parseStarted(current).traverse_(value => ready.complete(value).void)

  private def captureOwnedPid(
      output: Ref[IO, String],
      ownedPid: Deferred[IO, Int],
  )(chunk: String): IO[Unit] =
    output
      .updateAndGet(_ + chunk)
      .flatMap: current =>
        val completePid = OwnedPidPattern
          .findFirstMatchIn(current)
          .flatMap(_.group(1).toIntOption)
          .traverse_(value => ownedPid.complete(value).void)
        completePid

  private def captureFixedExample(
      output: Ref[IO, String],
      ownedPid: Deferred[IO, Int],
      ready: Deferred[IO, PreviewUrl],
  )(chunk: String): IO[Unit] =
    output
      .updateAndGet(_ + chunk)
      .flatMap: current =>
        val completePid = OwnedPidPattern
          .findFirstMatchIn(current)
          .flatMap(_.group(1).toIntOption)
          .traverse_(value => ownedPid.complete(value).void)
        val completeReady = FixedExampleReadyPattern
          .findFirstMatchIn(current)
          .traverse_(_ => ready.complete(exampleUrl).void)
        completePid *> completeReady

  private def captureDynamicExample(
      output: Ref[IO, String],
      ownedPid: Deferred[IO, Int],
      ready: Deferred[IO, PreviewUrl],
  )(chunk: String): IO[Unit] =
    output
      .updateAndGet(_ + chunk)
      .flatMap: current =>
        val completePid = OwnedPidPattern
          .findFirstMatchIn(current)
          .flatMap(_.group(1).toIntOption)
          .traverse_(value => ownedPid.complete(value).void)
        val completeReady = DynamicExampleReadyPattern
          .findFirstMatchIn(current)
          .flatMap(found => PreviewUrl.parse(found.group(1)).toOption)
          .traverse_(value => ready.complete(value).void)
        completePid *> completeReady

  private def parseStarted(value: String): Option[Started] =
    StartedPattern
      .findFirstMatchIn(value)
      .flatMap: found =>
        (found.group(1).toIntOption, found.group(2).toIntOption)
          .mapN(Started.apply)

  private def parseRoot(value: String): Option[Int] =
    RootPattern.findFirstMatchIn(value).flatMap(_.group(1).toIntOption)

  private def processIsAlive(pid: Int): IO[Boolean] =
    IO.delay(NodeRuntime.kill(pid, 0)).as(true).handleError(_ => false)

  private def phase[A](name: String, action: IO[A]): IO[A] =
    action.adaptError: error =>
      IllegalStateException(s"$name failed: ${error.getMessage}", error)

  private def command(script: String): PreviewCommand =
    val arguments = Vector("-e", script).zipWithIndex.map:
      case (value, index) => right(ProcessArgument.parse(value, index))
    PreviewCommand(
      executable = right(Executable.parse(TestNodeRuntime.execPath)),
      arguments = arguments,
      workingDirectory = right(
        WorkingDirectory.parseResolved(
          resolved = TestNodeRuntime.cwd(),
          relativeFromRoot = "",
          paths = nodePathSemantics,
        ),
      ),
      endpoint = PreviewEndpoint.Fixed(
        right(PreviewUrl.parse("http://127.0.0.1:4242")),
      ),
      startupTimeout = right(StartupTimeout.parse(5000)),
    )

  private val exampleCommand: PreviewCommand =
    val arguments = Vector(
      "--server",
      "--batch",
      "set doctrellisPort := 0",
      "doctrellisPreview",
    ).zipWithIndex.map:
      case (value, index) => right(ProcessArgument.parse(value, index))
    val directory = NodePath.join(TestNodeRuntime.cwd(), "examples", "basic")
    PreviewCommand(
      executable = right(Executable.parse("sbt")),
      arguments = arguments,
      workingDirectory = right(
        WorkingDirectory.parseResolved(
          resolved = directory,
          relativeFromRoot = "examples/basic",
          paths = nodePathSemantics,
        ),
      ),
      endpoint = PreviewEndpoint.Announced,
      startupTimeout = right(StartupTimeout.parse(120000)),
    )

  private val exampleFixedCommand: PreviewCommand =
    val arguments = Vector(
      "--server",
      "--batch",
      "doctrellisPreview",
    ).zipWithIndex.map:
      case (value, index) => right(ProcessArgument.parse(value, index))
    PreviewCommand(
      executable = right(Executable.parse("sbt")),
      arguments = arguments,
      workingDirectory = exampleCommand.workingDirectory,
      endpoint = PreviewEndpoint.Fixed(exampleUrl),
      startupTimeout = right(StartupTimeout.parse(120000)),
    )

  private val missingCommand: PreviewCommand =
    PreviewCommand(
      executable = right(
        Executable.parse("doctrellis-command-that-does-not-exist"),
      ),
      arguments = Vector.empty,
      workingDirectory = right(
        WorkingDirectory.parseResolved(
          resolved = TestNodeRuntime.cwd(),
          relativeFromRoot = "",
          paths = nodePathSemantics,
        ),
      ),
      endpoint = PreviewEndpoint.Fixed(exampleUrl),
      startupTimeout = right(StartupTimeout.parse(5000)),
    )

  private val exampleUrl: PreviewUrl =
    right(PreviewUrl.parse("http://127.0.0.1:4242"))

  private val runExampleIntegration: Boolean =
    NodeRuntime.env
      .get("DOCTRELLIS_RUN_EXAMPLE_INTEGRATION")
      .flatMap(_.toOption)
      .contains("1")

  private def serverScript(port: Int, ignoreTerm: Boolean): String =
    val signalHandler =
      if ignoreTerm then "process.on('SIGTERM', () => {});" else ""
    s"""
       |const http = require('node:http');
       |$signalHandler
       |const server = http.createServer((_, response) => response.end('ok'));
       |server.listen($port, '127.0.0.1', () => {
       |  const address = server.address();
       |  console.log(`READY:$${process.pid}:$${address.port}`);
       |});
       |""".stripMargin

  private val ignoringTreeScript: String =
    val child = serverScript(port = 0, ignoreTerm = true)
    s"""
       |const { spawn } = require('node:child_process');
       |process.on('SIGTERM', () => {});
       |const child = spawn(process.execPath, ['-e', ${jsString(child)}], {
       |  stdio: ['ignore', 'inherit', 'inherit']
       |});
       |console.log(`ROOT:$${process.pid}`);
       |setInterval(() => {}, 1000);
       |""".stripMargin

  private def jsString(value: String): String =
    scala.scalajs.js.JSON.stringify(value)

  private def right[A](value: Either[ConfigProblem, A]): A =
    value.fold(problem => fail(problem.message), identity)

  private final case class Started(pid: Int, port: Int):
    def url: String = s"http://127.0.0.1:$port"

  private val StartedPattern: Regex  = "READY:([0-9]+):([0-9]+)".r
  private val RootPattern: Regex     = "ROOT:([0-9]+)".r
  private val OwnedPidPattern: Regex = "Owned process tree PID: ([0-9]+)".r
  private val FixedExampleReadyPattern: Regex =
    "DocTrellis preview ready at http://127\\.0\\.0\\.1:4242".r
  private val DynamicExampleReadyPattern: Regex =
    "DocTrellis preview ready at (http://127\\.0\\.0\\.1:[0-9]+)".r
