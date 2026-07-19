package org.doctrellis.vscode.infrastructure

import cats.effect.std.Dispatcher
import cats.effect.{Deferred, IO, Ref}
import munit.CatsEffectSuite
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

import scala.concurrent.duration.*
import scala.scalajs.js

class WorkspaceChannelSuite extends CatsEffectSuite:
  private val firstUrl  = right(PreviewUrl.parse("http://127.0.0.1:43123"))
  private val secondUrl = right(PreviewUrl.parse("http://127.0.0.1:43124"))

  test("NDJSON framing handles every split and multiple LF or CRLF frames"):
    val frame    = readyFrame(firstUrl, requestId = "hello", pid = 42)
    val complete = frame + "\n"
    (0 to complete.length).foreach: split =>
      val decoder = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
      val frames  = for
        left  <- decoder.offer(complete.take(split))
        right <- decoder.offer(complete.drop(split))
      yield left ++ right
      assertEquals(frames, Right(Vector(frame)), clues(split))

    val decoder = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
    assertEquals(
      decoder.offer(s"$frame\r\n$frame\n"),
      Right(Vector(frame, frame)),
    )

  test("NDJSON framing accepts an exact maximum CRLF frame across chunks"):
    val payload = "x" * WorkspaceChannel.MaxFrameBytes
    val decoder = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
    assertEquals(decoder.offer(payload), Right(Vector.empty))
    assertEquals(decoder.offer("\r"), Right(Vector.empty))
    assertEquals(decoder.offer("\n"), Right(Vector(payload)))

  test("NDJSON framing handles many short frames in one chunk"):
    val count   = 4096
    val decoder = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)

    assertEquals(
      decoder.offer(Vector.fill(count)("x").mkString("", "\n", "\n")),
      Right(Vector.fill(count)("x")),
    )

  test("NDJSON framing rejects empty and oversized frames"):
    val empty = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
    assert(empty.offer("\n").isLeft)
    val oversized = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
    assert(oversized.offer("x" * (WorkspaceChannel.MaxFrameBytes + 1)).isLeft)

  test("NDJSON framing rejects an incrementally oversized pending frame"):
    val decoder = NdjsonFrameDecoder(4)
    assertEquals(decoder.offer("éé"), Right(Vector.empty))
    assertEquals(decoder.offer("x"), Left("IPC frame exceeds 4 bytes"))

    val afterCarriageReturn = NdjsonFrameDecoder(4)
    assertEquals(afterCarriageReturn.offer("xxxx\r"), Right(Vector.empty))
    assertEquals(
      afterCarriageReturn.offer("x"),
      Left("IPC frame exceeds 4 bytes"),
    )

  test("server protocol rejects malformed, unknown, and conflicting identity"):
    val valid = WorkspaceChannelProtocol.decodeServer(
      frame = readyFrame(firstUrl, requestId = "hello", pid = 42),
      helloRequestId = "hello",
      pingRequestId = None,
      stopRequestId = None,
      expectedSessionId = "session",
      expectedPid = 42,
      expectedLaunchId = "launch",
    )
    assertEquals(valid, Right(WorkspaceChannelMessage.Ready(firstUrl)))
    assert(
      WorkspaceChannelProtocol
        .decodeServer(
          frame = "not-json",
          helloRequestId = "hello",
          pingRequestId = None,
          stopRequestId = None,
          expectedSessionId = "session",
          expectedPid = 42,
          expectedLaunchId = "launch",
        )
        .isLeft,
    )
    assert(
      WorkspaceChannelProtocol
        .decodeServer(
          frame = readyFrame(firstUrl, requestId = "hello", pid = 42)
            .replace("\"launchId\":\"launch\"", "\"launchId\":\"other\""),
          helloRequestId = "hello",
          pingRequestId = None,
          stopRequestId = None,
          expectedSessionId = "session",
          expectedPid = 42,
          expectedLaunchId = "launch",
        )
        .isLeft,
    )
    assert(
      WorkspaceChannelProtocol
        .decodeServer(
          frame = readyFrame(firstUrl, requestId = "other", pid = 42),
          helloRequestId = "hello",
          pingRequestId = None,
          stopRequestId = None,
          expectedSessionId = "session",
          expectedPid = 42,
          expectedLaunchId = "launch",
        )
        .isLeft,
    )
    assert(
      WorkspaceChannelProtocol
        .decodeServer(
          frame = readyFrame(firstUrl, requestId = "hello", pid = 43),
          helloRequestId = "hello",
          pingRequestId = None,
          stopRequestId = None,
          expectedSessionId = "session",
          expectedPid = 42,
          expectedLaunchId = "launch",
        )
        .isLeft,
    )
    assert(
      WorkspaceChannelProtocol
        .decodeServer(
          frame = readyFrame(firstUrl, requestId = "hello", pid = 42)
            .replace("\"version\":1", "\"version\":2"),
          helloRequestId = "hello",
          pingRequestId = None,
          stopRequestId = None,
          expectedSessionId = "session",
          expectedPid = 42,
          expectedLaunchId = "launch",
        )
        .isLeft,
    )
    assert(
      WorkspaceChannelProtocol
        .decodeServer(
          frame = readyFrame(firstUrl, requestId = "hello", pid = 42)
            .replace("\"ready\"", "\"unknown\""),
          helloRequestId = "hello",
          pingRequestId = None,
          stopRequestId = None,
          expectedSessionId = "session",
          expectedPid = 42,
          expectedLaunchId = "launch",
        )
        .isLeft,
    )

  test("pong requires the outstanding ping request id"):
    val valid = WorkspaceChannelProtocol.decodeServer(
      frame = """{"version":1,"type":"pong","requestId":"ping-1"}""",
      helloRequestId = "hello",
      pingRequestId = Some("ping-1"),
      stopRequestId = None,
      expectedSessionId = "session",
      expectedPid = 42,
      expectedLaunchId = "launch",
    )
    val unknown = WorkspaceChannelProtocol.decodeServer(
      frame = """{"version":1,"type":"pong","requestId":"other"}""",
      helloRequestId = "hello",
      pingRequestId = Some("ping-1"),
      stopRequestId = None,
      expectedSessionId = "session",
      expectedPid = 42,
      expectedLaunchId = "launch",
    )
    assertEquals(valid, Right(WorkspaceChannelMessage.Pong))
    assert(unknown.isLeft)

  test("ready state rejects duplicate and conflicting announcements"):
    assertEquals(ReadyAnnouncement.validate(None, firstUrl), Right(firstUrl))
    assert(
      ReadyAnnouncement
        .validate(Some(firstUrl), firstUrl)
        .left
        .exists(
          _.contains("duplicate"),
        ),
    )
    assert(
      ReadyAnnouncement
        .validate(Some(firstUrl), secondUrl)
        .left
        .exists(
          _.contains("conflicting"),
        ),
    )

  test("metadata requires the expected endpoint, loopback, and token"):
    val endpoint = WorkspaceChannelEndpoint(
      canonicalWorkspace = "/workspace",
      runtimeDirectory = "/tmp/doctrellis-user",
      metadataPath = "/tmp/doctrellis-user/workspace.json",
      socketPrefix = "/tmp/doctrellis-user/workspace.",
      workspaceHash = "workspace",
      runtimeOwner = Some(501),
    )
    val valid =
      """{"version":1,"protocol":"doctrellis-preview-v1","transport":"unix","address":"/tmp/doctrellis-user/workspace.0123456789abcdef.sock","pid":42,"sessionId":"session","launchId":"launch","token":"0123456789abcdef0123456789abcdef"}"""
    assert(WorkspaceChannelMetadataParser.parse(valid, endpoint).isRight)
    assertEquals(WorkspaceChannelMetadataParser.ownerPid(valid), Right(42))
    assert(
      WorkspaceChannelMetadataParser
        .parse(
          valid.replace("workspace.0123456789abcdef.sock", "other.sock"),
          endpoint,
        )
        .isLeft,
    )
    assert(
      WorkspaceChannelMetadataParser
        .parse(
          valid.replace("0123456789abcdef0123456789abcdef", "short"),
          endpoint,
        )
        .isLeft,
    )
    val tcp = valid
      .replace("\"unix\"", "\"tcp\"")
      .replace(
        "\"address\":\"/tmp/doctrellis-user/workspace.0123456789abcdef.sock\"",
        "\"host\":\"0.0.0.0\",\"port\":43123",
      )
    assert(WorkspaceChannelMetadataParser.parse(tcp, endpoint).isLeft)

  test("an owned preview rejects metadata from a competing launch"):
    promise(
      TestNodeFsPromises.mkdtemp(
        NodePath.join(NodeOs.tmpdir(), "doctrellis-launch-correlation-"),
      ),
    ).bracket(temporary =>
      val metadataPath = NodePath.join(temporary, "workspace.json")
      val endpoint     = WorkspaceChannelEndpoint(
        canonicalWorkspace = "/workspace",
        runtimeDirectory = temporary,
        metadataPath = metadataPath,
        socketPrefix = NodePath.join(temporary, "workspace."),
        workspaceHash = "workspace",
        runtimeOwner = None,
      )
      val metadata =
        s"""{"version":1,"protocol":"${WorkspaceChannel.Protocol}","transport":"tcp","host":"127.0.0.1","port":9,"pid":42,"sessionId":"winner-session","launchId":"winner-launch","token":"${"0" * 64}"}"""
      for
        _      <- promise(TestNodeFsPromises.writeFile(metadataPath, metadata))
        result <- Dispatcher
          .parallel[IO]
          .use: dispatcher =>
            WorkspaceChannel
              .connectReady(
                dispatcher = dispatcher,
                endpoint = endpoint,
                verifyPing = true,
                expectedLaunchId = Some("loser-launch"),
              )
              .attempt
      yield assert(
        result.left.exists(_.getMessage.contains("different preview launch")),
      ),
    )(temporary =>
      promise(
        TestNodeFsPromises.rm(
          temporary,
          js.Dynamic.literal(recursive = true, force = true),
        ),
      ),
    )

  test("closing a connection settles pending ready and ping waits"):
    Dispatcher
      .parallel[IO]
      .use: dispatcher =>
        val metadata = WorkspaceChannelMetadata(
          transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
          pid = 42,
          sessionId = "session",
          launchId = "launch",
          token = "0" * 64,
        )
        for
          connection <- WorkspaceChannelConnection.create(
            dispatcher = dispatcher,
            socket = inertSocket(),
            metadata = metadata,
          )
          ready       <- connection.awaitUrl.attempt.start
          pong        <- connection.ping.attempt.start
          _           <- IO.cede
          _           <- connection.close
          readyResult <- ready.joinWithNever.timeout(1.second)
          pongResult  <- pong.joinWithNever.timeout(1.second)
        yield
          assert(readyResult.isLeft)
          assert(pongResult.isLeft)

  test("reinstalling a shared connection does not close it"):
    Dispatcher
      .parallel[IO]
      .use: dispatcher =>
        val metadata = WorkspaceChannelMetadata(
          transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
          pid = 42,
          sessionId = "session",
          launchId = "launch",
          token = "0" * 64,
        )
        val (socket, destroyCount) = trackedInertSocket()
        for
          connection <- WorkspaceChannelConnection.create(
            dispatcher = dispatcher,
            socket = socket,
            metadata = metadata,
          )
          state <- Ref.of[IO, DynamicPreviewConnection](
            DynamicPreviewConnection.Pending,
          )
          first    <- DynamicPreviewRuntime.installConnection(state, connection)
          second   <- DynamicPreviewRuntime.installConnection(state, connection)
          destroys <- IO.delay(destroyCount())
          _        <- connection.close
        yield
          assert(first.eq(connection))
          assert(second.eq(connection))
          assertEquals(destroys, 0)

  test(
    "releasing a reused runtime closes a connection acquired at the stop boundary",
  ):
    Dispatcher
      .parallel[IO]
      .use: dispatcher =>
        val metadata = WorkspaceChannelMetadata(
          transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
          pid = 42,
          sessionId = "session",
          launchId = "launch",
          token = "0" * 64,
        )
        val (socket, destroyCount) = trackedInertSocket()
        for
          connection <- WorkspaceChannelConnection.create(
            dispatcher = dispatcher,
            socket = socket,
            metadata = metadata,
          )
          acquireEntered  <- Deferred[IO, Unit]
          continueAcquire <- Deferred[IO, Unit]
          runtime         <- DynamicPreviewRuntime.reused(
            IO.uncancelable: _ =>
              acquireEntered.complete(()).void *>
                continueAcquire.get.as(connection),
          )
          awaitUrl <- runtime.running.announcedUrl match
            case Some(wait) => wait.attempt.start
            case None       =>
              IO.raiseError(
                IllegalStateException("expected a dynamic preview URL wait"),
              )
          _        <- acquireEntered.get
          _        <- runtime.release
          _        <- continueAcquire.complete(())
          result   <- awaitUrl.joinWithNever.timeout(1.second)
          destroys <- IO.delay(destroyCount())
        yield
          assert(result.isLeft)
          assertEquals(destroys, 1)

  test("workspace endpoints are stable, distinct, short, and outside projects"):
    val root  = TestNodeRuntime.cwd()
    val first = Workspace(
      key = "first",
      name = "first",
      root = root,
      uri = "file://first",
    )
    val second = Workspace(
      key = "second",
      name = "second",
      root = NodePath.join(root, "modules"),
      uri = "file://second",
    )
    for
      firstEndpoint  <- WorkspaceChannel.endpoint(first)
      repeated       <- WorkspaceChannel.endpoint(first)
      secondEndpoint <- WorkspaceChannel.endpoint(second)
    yield
      assertEquals(firstEndpoint, repeated)
      assertNotEquals(firstEndpoint.workspaceHash, secondEndpoint.workspaceHash)
      if ProcessPlatform.parse(NodeRuntime.platform) == ProcessPlatform.Posix
      then
        assert(
          NodeBuffer.Buffer.byteLength(
            s"${firstEndpoint.socketPrefix}${"f" * 16}.sock",
            "utf8",
          ) <= 100,
        )
      assert(!firstEndpoint.runtimeDirectory.startsWith(first.root))
      assert(!secondEndpoint.runtimeDirectory.startsWith(second.root))

  test("runtime validation rejects symlinks and a different POSIX owner"):
    if ProcessPlatform.parse(NodeRuntime.platform) == ProcessPlatform.Windows
    then IO.unit
    else
      promise(
        TestNodeFsPromises.mkdtemp(
          NodePath.join(NodeOs.tmpdir(), "doctrellis-runtime-check-"),
        ),
      ).bracket(temporary =>
        val target = NodePath.join(temporary, "target")
        val link   = NodePath.join(temporary, "link")
        for
          _ <- promise(
            TestNodeFsPromises.mkdir(
              target,
              js.Dynamic.literal(recursive = true),
            ),
          )
          _       <- promise(TestNodeFsPromises.symlink(target, link))
          symlink <- WorkspaceChannel
            .verifyRuntimeDirectory(link, NodeOs.userInfo().uid)
            .attempt
          wrongOwner <- WorkspaceChannel
            .verifyRuntimeDirectory(target, NodeOs.userInfo().uid + 1)
            .attempt
        yield
          assert(symlink.isLeft)
          assert(wrongOwner.isLeft),
      )(temporary =>
        promise(
          TestNodeFsPromises.rm(
            temporary,
            js.Dynamic.literal(recursive = true, force = true),
          ),
        ),
      )

  test("IPC disconnect falls back to exact process release"):
    for
      events <- Ref.of[IO, Vector[String]](Vector.empty)
      _      <- DynamicPreviewShutdown.release(
        gracefulStop = IO.raiseError(IllegalStateException("disconnected")),
        awaitProcessExit = IO.never,
        closeChannel = events.update(_ :+ "close-channel"),
        releaseProcess = events.update(_ :+ "release-process"),
        timeout = 50.millis,
        onFallback =
          error => events.update(_ :+ s"fallback:${error.getMessage}"),
      )
      actual <- events.get
    yield assertEquals(
      actual,
      Vector("fallback:disconnected", "close-channel", "release-process"),
    )

  test("graceful IPC stop waits for process exit before final release"):
    for
      exited      <- Deferred[IO, Unit]
      stopEntered <- Deferred[IO, Unit]
      events      <- Ref.of[IO, Vector[String]](Vector.empty)
      release     <- DynamicPreviewShutdown
        .release(
          gracefulStop = events.update(_ :+ "stopped") *>
            stopEntered.complete(()).void,
          awaitProcessExit = exited.get,
          closeChannel = events.update(_ :+ "close-channel"),
          releaseProcess = events.update(_ :+ "release-process"),
          timeout = 1.second,
          onFallback = _ => events.update(_ :+ "fallback"),
        )
        .start
      _      <- stopEntered.get
      before <- events.get
      _      <- exited.complete(())
      _      <- release.joinWithNever
      after  <- events.get
    yield
      assertEquals(before, Vector("stopped"))
      assertEquals(
        after,
        Vector("stopped", "close-channel", "release-process"),
      )

  private def readyFrame(
      url: PreviewUrl,
      requestId: String,
      pid: Int,
  ): String =
    s"""{"version":1,"type":"ready","requestId":"$requestId","sessionId":"session","launchId":"launch","pid":$pid,"url":"${url.value}"}"""

  private def promise[A](value: => js.Promise[A]): IO[A] =
    IO.fromPromise(IO.delay(value))

  private def inertSocket(): NodeSocket =
    trackedInertSocket()._1

  private def trackedInertSocket(): (NodeSocket, () => Int) =
    val socket       = js.Dynamic.literal()
    var destroys     = 0
    val returnSocket =
      ((_: String, _: js.Function) => socket): js.Function2[
        String,
        js.Function,
        js.Dynamic,
      ]
    socket.updateDynamic("on")(returnSocket)
    socket.updateDynamic("once")(returnSocket)
    socket.updateDynamic("off")(returnSocket)
    socket.updateDynamic("write")(
      ((_: String) => true): js.Function1[String, Boolean],
    )
    socket.updateDynamic("setEncoding")(
      ((_: String) => socket): js.Function1[String, js.Dynamic],
    )
    socket.updateDynamic("end")((() => ()): js.Function0[Unit])
    socket.updateDynamic("destroy")(
      (() => destroys += 1): js.Function0[Unit],
    )
    socket.asInstanceOf[NodeSocket] -> (() => destroys)

  private def right[A](value: Either[ConfigProblem, A]): A =
    value.fold(problem => fail(problem.message), identity)
