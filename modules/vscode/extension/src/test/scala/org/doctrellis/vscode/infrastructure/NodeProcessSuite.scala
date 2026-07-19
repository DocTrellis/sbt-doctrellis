package org.doctrellis.vscode.infrastructure

import cats.effect.std.Dispatcher
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.doctrellis.vscode.application.RunningProcess
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.{
  NodePath,
  NodeRuntime,
  TestNodeRuntime,
}

import scala.concurrent.duration.*
import scala.util.matching.Regex

class NodeProcessSuite extends CatsEffectSuite:
  private val workspace = Workspace(
    key = "file:///node-process-suite",
    name = "node-process-suite",
    root = TestNodeRuntime.cwd(),
    uri = "file:///node-process-suite",
  )

  test("stops a ready process, releases its port, and tolerates repeat stop"):
    Dispatcher.parallel[IO](await = false).use(testReadyStop)

  test("forces only its TERM-ignoring POSIX process group"):
    if ProcessPlatform.parse(NodeRuntime.platform) == ProcessPlatform.Windows
    then IO.unit
    else Dispatcher.parallel[IO](await = false).use(testForcedTree)

  test("selects the exact Windows task tree command"):
    IO:
      assertEquals(ProcessPlatform.parse("win32"), ProcessPlatform.Windows)
      assertEquals(ProcessPlatform.parse("linux"), ProcessPlatform.Posix)
      assertEquals(
        OwnedProcess.taskkillArguments(pid = 1234, force = false),
        Vector("/PID", "1234", "/T"),
      )
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
      assertEquals(exit.error, Some("close notification was not delivered"))
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

  test("stops the actual basic sbt preview without leaving its listener"):
    if !runExampleIntegration then IO.unit
    else Dispatcher.parallel[IO](await = false).use(testExampleLifecycle)

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
      if ProcessPlatform.parse(NodeRuntime.platform) == ProcessPlatform.Posix
      then
        assertEquals(exit.code, None)
        assertEquals(exit.signal, Some("SIGTERM"))
      else assert(exit.code.nonEmpty || exit.signal.nonEmpty)
      assert(!stopped)
      assert(rebound)
      assert(!alive)

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
      assertEquals(exit.signal, Some("SIGKILL"))
      assert(!childReady)
      assert(otherReady)
      assert(!rootAlive)
      assert(!childAlive)
      assert(logs.contains("forcing its owned process tree"))

  private def testExampleLifecycle(dispatcher: Dispatcher[IO]): IO[Unit] =
    for
      ready        <- Deferred[IO, PreviewUrl]
      ownedPid     <- Deferred[IO, Int]
      log          <- Ref.of[IO, String]("")
      activeBefore <- NodeHttpProbe.ready(exampleUrl)
      _            <- IO.raiseWhen(activeBefore)(
        IllegalStateException("port 4242 was already in use"),
      )
      result <- managedProcess(
        dispatcher = dispatcher,
        command = exampleCommand,
        append = captureExample(log, ownedPid, ready),
        timeouts = ProcessShutdownTimeouts.default,
      ).use:
        case (running, stop) =>
          for
            url     <- ready.get.timeout(120.seconds)
            pid     <- ownedPid.get.timeout(5.seconds)
            served  <- NodeHttpProbe.ready(url)
            _       <- stop
            exit    <- running.awaitExit
            stopped <- NodeHttpProbe.ready(url)
            alive   <- processIsAlive(pid)
            rebound <- rebind(dispatcher, port = 4242)
          yield (served, exit, stopped, alive, rebound)
      (served, exit, stopped, alive, rebound) = result
    yield
      assert(served)
      assert(exit.code.nonEmpty || exit.signal.nonEmpty)
      assert(!stopped)
      assert(!alive)
      assert(rebound)

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

  private def captureExample(
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
        val completeReady = ExampleReadyPattern
          .findFirstMatchIn(current)
          .traverse_(_ => ready.complete(exampleUrl).void)
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
          separator = NodePath.sep,
          relativeIsAbsolute = false,
        ),
      ),
      url = right(PreviewUrl.parse("http://127.0.0.1:4242")),
      startupTimeout = right(StartupTimeout.parse(5000)),
    )

  private val exampleCommand: PreviewCommand =
    val arguments = Vector(
      "--server",
      "--batch",
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
          separator = NodePath.sep,
          relativeIsAbsolute = false,
        ),
      ),
      url = exampleUrl,
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
          separator = NodePath.sep,
          relativeIsAbsolute = false,
        ),
      ),
      url = exampleUrl,
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

  private val StartedPattern: Regex      = "READY:([0-9]+):([0-9]+)".r
  private val RootPattern: Regex         = "ROOT:([0-9]+)".r
  private val OwnedPidPattern: Regex     = "Owned process tree PID: ([0-9]+)".r
  private val ExampleReadyPattern: Regex =
    "DocTrellis preview ready at http://127\\.0\\.0\\.1:4242".r
