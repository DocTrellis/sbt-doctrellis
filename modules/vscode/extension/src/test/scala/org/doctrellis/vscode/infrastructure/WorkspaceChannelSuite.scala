package org.doctrellis.vscode.infrastructure

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

  test("NDJSON framing rejects empty and oversized frames"):
    val empty = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
    assert(empty.offer("\n").isLeft)
    val oversized = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
    assert(oversized.offer("x" * (WorkspaceChannel.MaxFrameBytes + 1)).isLeft)

  test("server protocol rejects malformed, unknown, and conflicting identity"):
    val valid = WorkspaceChannelProtocol.decodeServer(
      frame = readyFrame(firstUrl, requestId = "hello", pid = 42),
      helloRequestId = "hello",
      pingRequestId = None,
      stopRequestId = None,
      expectedSessionId = "session",
      expectedPid = 42,
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
    )
    val unknown = WorkspaceChannelProtocol.decodeServer(
      frame = """{"version":1,"type":"pong","requestId":"other"}""",
      helloRequestId = "hello",
      pingRequestId = Some("ping-1"),
      stopRequestId = None,
      expectedSessionId = "session",
      expectedPid = 42,
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
      """{"version":1,"protocol":"doctrellis-preview-v1","transport":"unix","address":"/tmp/doctrellis-user/workspace.0123456789abcdef.sock","pid":42,"sessionId":"session","token":"0123456789abcdef0123456789abcdef"}"""
    assert(WorkspaceChannelMetadataParser.parse(valid, endpoint).isRight)
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
    s"""{"version":1,"type":"ready","requestId":"$requestId","sessionId":"session","pid":$pid,"url":"${url.value}"}"""

  private def promise[A](value: => js.Promise[A]): IO[A] =
    IO.fromPromise(IO.delay(value))

  private def right[A](value: Either[ConfigProblem, A]): A =
    value.fold(problem => fail(problem.message), identity)
