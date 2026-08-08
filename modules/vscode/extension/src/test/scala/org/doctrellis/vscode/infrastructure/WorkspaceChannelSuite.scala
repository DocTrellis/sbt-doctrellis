package org.doctrellis.vscode.infrastructure

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array

import cats.effect.{Deferred, IO, Ref}

import munit.CatsEffectSuite

import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

class WorkspaceChannelSuite extends CatsEffectSuite:
  private val firstUrl  = right(PreviewUrl.parse("http://127.0.0.1:43123"))
  private val secondUrl = right(PreviewUrl.parse("http://127.0.0.1:43124"))
  private val currentPlatform = ProcessPlatform
    .parse(NodeRuntime.platform)
    .fold(problem => fail(problem.getMessage), identity)

  test("hello opts into site events with a numeric atomic field"):
    assertEquals(
      WorkspaceChannelProtocol.hello("token", "client", "request"),
      """{"version":1,"type":"hello","siteEvents":1,"token":"token","clientId":"client","requestId":"request"}
        |""".stripMargin,
    )

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

  test(
    "NDJSON framing rejects malformed UTF-8 without losing split characters",
  ):
    val valid  = Utf8NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
    val first  = bytes(0x22, 0xc3)
    val second = bytes(0xa9, 0x22, 0x0a)
    assertEquals(valid.offer(first), Right(Vector.empty))
    assertEquals(valid.offer(second), Right(Vector("\"é\"")))

    val malformed = Utf8NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
    assertEquals(
      malformed.offer(bytes(0x22, 0xc3, 0x28, 0x22, 0x0a)),
      Left("IPC frame must contain valid UTF-8"),
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

  test("error responses require their complete wire shape"):
    val correlated = WorkspaceChannelProtocol.decodeServer(
      frame =
        """{"version":1,"type":"error","requestId":"hello","code":"authenticationFailed","message":"denied"}""",
      helloRequestId = "hello",
      pingRequestId = None,
      stopRequestId = None,
      expectedSessionId = "session",
      expectedPid = 42,
      expectedLaunchId = "launch",
    )
    val uncorrelated = WorkspaceChannelProtocol.decodeServer(
      frame =
        """{"version":1,"type":"error","requestId":"","code":"invalidFrame","message":"IPC frame exceeds 8192 bytes"}""",
      helloRequestId = "hello",
      pingRequestId = None,
      stopRequestId = None,
      expectedSessionId = "session",
      expectedPid = 42,
      expectedLaunchId = "launch",
    )
    assertEquals(correlated, Right(WorkspaceChannelMessage.Error("denied")))
    assertEquals(
      uncorrelated,
      Right(WorkspaceChannelMessage.Error("IPC frame exceeds 8192 bytes")),
    )

    val invalid = Vector(
      """{"version":1,"type":"error","requestId":"other","code":"authenticationFailed","message":"denied"}""",
      """{"version":1,"type":"error","code":"authenticationFailed","message":"denied"}""",
      """{"version":1,"type":"error","requestId":"hello","message":"denied"}""",
      """{"version":1,"type":"error","requestId":"hello","code":"","message":"denied"}""",
      """{"version":1,"type":"error","requestId":"hello","code":"authenticationFailed","message":""}""",
    )
    invalid.foreach: frame =>
      assert(
        WorkspaceChannelProtocol
          .decodeServer(
            frame = frame,
            helloRequestId = "hello",
            pingRequestId = None,
            stopRequestId = None,
            expectedSessionId = "session",
            expectedPid = 42,
            expectedLaunchId = "launch",
          )
          .isLeft,
        clues(frame),
      )

  test("site change messages parse a matching session and safe revision"):
    val valid = WorkspaceChannelProtocol.decodeServer(
      frame = siteChangedFrame("session", 2),
      helloRequestId = "hello",
      pingRequestId = None,
      stopRequestId = None,
      expectedSessionId = "session",
      expectedPid = 42,
      expectedLaunchId = "launch",
    )
    assertEquals(
      valid,
      Right(WorkspaceChannelMessage.SiteChanged(revision(2))),
    )

    val rejected = Vector(
      siteChangedFrame("other", 2),
      siteChangedFrame("session", 0),
      siteChangedFrame("session", -1),
      siteChangedFrame("session", 2)
        .replace("\"revision\":2", "\"revision\":2.5"),
      siteChangedFrame("session", 2).replace(
        "\"revision\":2",
        "\"revision\":9007199254740992",
      ),
    )
    rejected.foreach: frame =>
      assert(
        WorkspaceChannelProtocol
          .decodeServer(
            frame = frame,
            helloRequestId = "hello",
            pingRequestId = None,
            stopRequestId = None,
            expectedSessionId = "session",
            expectedPid = 42,
            expectedLaunchId = "launch",
          )
          .isLeft,
        clues(frame),
      )

  test("site revisions must increase within one connection"):
    val second = revision(2)
    val third  = revision(3)

    assertEquals(SiteRevisionAcceptance.validate(None, second), Right(second))
    assertEquals(
      SiteRevisionAcceptance.validate(Some(second), third),
      Right(third),
    )
    assert(SiteRevisionAcceptance.validate(Some(second), second).isLeft)
    assert(SiteRevisionAcceptance.validate(Some(third), second).isLeft)

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
        result <- WorkspaceChannel
          .connectReady(
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

  test("closing a connection ends site events and settles pending waits"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    for
      connection <- WorkspaceChannelConnection.create(
        socket = inertSocket(),
        metadata = metadata,
      )
      ready          <- connection.awaitUrl.attempt.start
      pong           <- connection.ping.attempt.start
      revision       <- connection.nextSiteRevision.attempt.start
      _              <- IO.cede
      _              <- connection.close *> connection.close
      readyResult    <- ready.joinWithNever.timeout(1.second)
      pongResult     <- pong.joinWithNever.timeout(1.second)
      revisionResult <- revision.joinWithNever.timeout(1.second)
    yield
      assert(readyResult.isLeft)
      assert(pongResult.isLeft)
      assertEquals(revisionResult, Right(None))

  test("initialization failure cleans the socket before returning"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, lifecycle) = failingInitializationSocket()
    for
      result <- WorkspaceChannelConnection
        .create(socket = socket, metadata = metadata)
        .attempt
      events <- IO.delay(lifecycle())
    yield
      assert(result.left.exists(_.getMessage.contains("write failed")))
      assert(events.contains("off:data"))
      assert(events.contains("off:error"))
      assert(events.contains("off:close"))
      assert(!events.contains("emit:close"))
      assert(events.indexOf("off:close") < events.indexOf("destroy"))

  test("a connection preserves an uncorrelated protocol error diagnostic"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, emitData, _, _, _) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          _ <- IO.delay(
            emitData(
              """{"version":1,"type":"error","requestId":"","code":"invalidFrame","message":"IPC frame exceeds 8192 bytes"}
                |""".stripMargin,
            ),
          )
          result <- connection.awaitExit.attempt.timeout(1.second)
        yield assert(
          result.left.exists(
            _.getMessage == "IPC frame exceeds 8192 bytes",
          ),
        ),
      )(_.close)

  test("a connection preserves event order and rejects a duplicate revision"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, emitData, _, _, writes) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          initialWrites <- IO.delay(writes())
          _             <- IO.delay(
            emitData(siteChangedFrame("session", 2) + "\n"),
          )
          _ <- IO.delay(
            emitData(siteChangedFrame("session", 3) + "\n"),
          )
          first  <- connection.nextSiteRevision.timeout(1.second)
          second <- connection.nextSiteRevision.timeout(1.second)
          _      <- IO.delay(
            emitData(siteChangedFrame("session", 3) + "\n"),
          )
          failure <- connection.nextSiteRevision.attempt.timeout(1.second)
          exit    <- connection.awaitExit.attempt.timeout(1.second)
        yield
          assert(initialWrites.exists(_.contains("\"siteEvents\":1")))
          assertEquals(
            Vector(first.map(_.value), second.map(_.value)),
            Vector(Some(2L), Some(3L)),
          )
          assert(failure.left.exists(_.getMessage.contains("must be greater")))
          assert(exit.left.exists(_.getMessage.contains("must be greater"))),
      )(_.close)

  test("a connection rejects a duplicate pong response"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, emitData, _, _, writes) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          ping      <- connection.ping.start
          _         <- IO.cede
          pingFrame <- IO.fromOption(
            writes().find(_.contains("\"type\":\"ping\"")),
          )(IllegalStateException("the ping request was not written"))
          requestId <- IO.delay(
            js.JSON
              .parse(pingFrame)
              .asInstanceOf[js.Dynamic]
              .selectDynamic("requestId")
              .asInstanceOf[String],
          )
          pongFrame =
            s"""{"version":1,"type":"pong","requestId":"$requestId"}\n"""
          _      <- IO.delay(emitData(pongFrame))
          _      <- ping.joinWithNever.timeout(1.second)
          _      <- IO.delay(emitData(pongFrame))
          result <- connection.awaitExit.attempt.timeout(1.second)
        yield assert(
          result.left.exists(
            _.getMessage.contains("duplicate or late pong response"),
          ),
        ),
      )(_.close)

  test("a connection rejects a duplicate stopped response"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, emitData, _, _, writes) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          stop      <- connection.stop.start
          _         <- IO.cede
          stopFrame <- IO.fromOption(
            writes().find(_.contains("\"type\":\"stop\"")),
          )(IllegalStateException("the stop request was not written"))
          requestId <- IO.delay(
            js.JSON
              .parse(stopFrame)
              .asInstanceOf[js.Dynamic]
              .selectDynamic("requestId")
              .asInstanceOf[String],
          )
          stoppedFrame =
            s"""{"version":1,"type":"stopped","requestId":"$requestId"}\n"""
          _      <- IO.delay(emitData(stoppedFrame))
          _      <- stop.joinWithNever.timeout(1.second)
          _      <- IO.delay(emitData(stoppedFrame))
          result <- connection.nextSiteRevision.attempt.timeout(1.second)
        yield assert(
          result.left.exists(
            _.getMessage.contains("duplicate or late stopped response"),
          ),
        ),
      )(_.close)

  test("a connection completes a stopping then stopped sequence"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, emitData, _, _, writes) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          stop      <- connection.stop.start
          _         <- IO.cede
          stopFrame <- IO.fromOption(
            writes().find(_.contains("\"type\":\"stop\"")),
          )(IllegalStateException("the stop request was not written"))
          requestId <- IO.delay(
            js.JSON
              .parse(stopFrame)
              .asInstanceOf[js.Dynamic]
              .selectDynamic("requestId")
              .asInstanceOf[String],
          )
          _ <- IO.delay(
            emitData(
              s"""{"version":1,"type":"stopping","requestId":"$requestId"}\n""",
            ),
          )
          _ <- IO.cede
          _ <- IO.delay(
            emitData(
              s"""{"version":1,"type":"stopped","requestId":"$requestId"}\n""",
            ),
          )
          result <- stop.joinWithNever.attempt.timeout(1.second)
          exit   <- connection.awaitExit.timeout(1.second)
        yield
          assertEquals(result, Right(()))
          assertEquals(
            exit,
            ProcessExit.Terminated(ProcessTermination.Code(0)),
          ),
      )(_.close)

  test("a connection rejects a duplicate stopping response"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, emitData, _, _, writes) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          stop      <- connection.stop.start
          _         <- IO.cede
          stopFrame <- IO.fromOption(
            writes().find(_.contains("\"type\":\"stop\"")),
          )(IllegalStateException("the stop request was not written"))
          requestId <- IO.delay(
            js.JSON
              .parse(stopFrame)
              .asInstanceOf[js.Dynamic]
              .selectDynamic("requestId")
              .asInstanceOf[String],
          )
          stoppingFrame =
            s"""{"version":1,"type":"stopping","requestId":"$requestId"}\n"""
          _      <- IO.delay(emitData(stoppingFrame))
          _      <- IO.delay(emitData(stoppingFrame))
          result <- stop.joinWithNever.attempt.timeout(1.second)
        yield assert(
          result.left.exists(
            _.getMessage.contains("duplicate or late stopping response"),
          ),
        ),
      )(_.close)

  test("a connection rejects a stopping response after stopped"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, emitData, _, _, writes) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          stop      <- connection.stop.start
          _         <- IO.cede
          stopFrame <- IO.fromOption(
            writes().find(_.contains("\"type\":\"stop\"")),
          )(IllegalStateException("the stop request was not written"))
          requestId <- IO.delay(
            js.JSON
              .parse(stopFrame)
              .asInstanceOf[js.Dynamic]
              .selectDynamic("requestId")
              .asInstanceOf[String],
          )
          _ <- IO.delay(
            emitData(
              s"""{"version":1,"type":"stopped","requestId":"$requestId"}\n""",
            ),
          )
          _ <- stop.joinWithNever.timeout(1.second)
          _ <- IO.delay(
            emitData(
              s"""{"version":1,"type":"stopping","requestId":"$requestId"}\n""",
            ),
          )
          result <- connection.nextSiteRevision.attempt.timeout(1.second)
        yield assert(
          result.left.exists(
            _.getMessage.contains("duplicate or late stopping response"),
          ),
        ),
      )(_.close)

  test("a transport error fails both site events and connection exit"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, _, emitError, emitClose, _) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          revision       <- connection.nextSiteRevision.attempt.start
          exit           <- connection.awaitExit.attempt.start
          _              <- IO.delay(emitError(new js.Error("transport lost")))
          _              <- IO.delay(emitClose())
          revisionResult <- revision.joinWithNever.timeout(1.second)
          exitResult     <- exit.joinWithNever.timeout(1.second)
        yield
          assert(
            revisionResult.left.exists(
              _.getMessage.contains("transport lost"),
            ),
          )
          assert(
            exitResult.left.exists(_.getMessage.contains("transport lost")),
          ),
      )(_.close)

  test("a normal transport end closes site events without a failure"):
    val metadata = WorkspaceChannelMetadata(
      transport = WorkspaceChannelTransport.Tcp("127.0.0.1", 43123),
      pid = 42,
      sessionId = "session",
      launchId = "launch",
      token = "0" * 64,
    )
    val (socket, _, _, emitClose, _) = controlledSocket()
    WorkspaceChannelConnection
      .create(socket = socket, metadata = metadata)
      .bracket(connection =>
        for
          revision       <- connection.nextSiteRevision.attempt.start
          exit           <- connection.awaitExit.attempt.start
          _              <- IO.delay(emitClose())
          revisionResult <- revision.joinWithNever.timeout(1.second)
          exitResult     <- exit.joinWithNever.timeout(1.second)
        yield
          assertEquals(revisionResult, Right(None))
          assert(
            exitResult.exists(
              _ == ProcessExit.Failed(
                "workspace IPC connection ended",
                termination = None,
              ),
            ),
          ),
      )(_.close)

  test("reinstalling a shared connection does not close it"):
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
      key = right(WorkspaceId.parse("file:///first")),
      name = "first",
      root = root,
    )
    val second = Workspace(
      key = right(WorkspaceId.parse("file:///second")),
      name = "second",
      root = NodePath.join(root, "modules"),
    )
    for
      firstEndpoint  <- WorkspaceChannel.endpoint(first)
      repeated       <- WorkspaceChannel.endpoint(first)
      secondEndpoint <- WorkspaceChannel.endpoint(second)
    yield
      assertEquals(firstEndpoint, repeated)
      assertNotEquals(firstEndpoint.workspaceHash, secondEndpoint.workspaceHash)
      if currentPlatform == ProcessPlatform.Posix then
        assert(
          NodeBuffer.Buffer.byteLength(
            s"${firstEndpoint.socketPrefix}${"f" * 16}.sock",
            "utf8",
          ) <= 100,
        )
      assert(!firstEndpoint.runtimeDirectory.startsWith(first.root))
      assert(!secondEndpoint.runtimeDirectory.startsWith(second.root))

  test("runtime validation rejects symlinks and a different POSIX owner"):
    if currentPlatform == ProcessPlatform.Windows then IO.unit
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

  private def siteChangedFrame(sessionId: String, value: Long): String =
    s"""{"version":1,"type":"siteChanged","sessionId":"$sessionId","revision":$value}"""

  private def revision(value: Long): SiteRevision =
    SiteRevision.parse(value).fold(problem => fail(problem.message), identity)

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
    socket.updateDynamic("destroy")(
      (() => destroys += 1): js.Function0[Unit],
    )
    socket.asInstanceOf[NodeSocket] -> (() => destroys)

  private def controlledSocket(): (
      NodeSocket,
      String => Unit,
      js.Any => Unit,
      () => Unit,
      () => Vector[String],
  ) =
    val socket   = js.Dynamic.literal()
    val writes   = mutable.ListBuffer.empty[String]
    var data     = Option.empty[js.Function1[Uint8Array, Unit]]
    var error    = Option.empty[js.Function1[js.Any, Unit]]
    var close    = Option.empty[js.Function0[Unit]]
    val register =
      (
          (event: String, listener: js.Function) =>
            event match
              case "data" =>
                data = Some(
                  listener.asInstanceOf[js.Function1[Uint8Array, Unit]],
                )
              case "error" =>
                error = Some(listener.asInstanceOf[js.Function1[js.Any, Unit]])
              case "close" =>
                close = Some(listener.asInstanceOf[js.Function0[Unit]])
              case _ => ()
            socket,
      ): js.Function2[String, js.Function, js.Dynamic]
    socket.updateDynamic("on")(register)
    socket.updateDynamic("once")(register)
    socket.updateDynamic("off")(
      ((_: String, _: js.Function) => socket): js.Function2[
        String,
        js.Function,
        js.Dynamic,
      ],
    )
    socket.updateDynamic("write")(
      ((value: String) =>
        writes += value
        true,
      ): js.Function1[String, Boolean],
    )
    socket.updateDynamic("destroy")((() => ()): js.Function0[Unit])
    (
      socket.asInstanceOf[NodeSocket],
      chunk => data.foreach(callback => callback(asciiBytes(chunk))),
      value => error.foreach(callback => callback(value)),
      () => close.foreach(callback => callback()),
      () => writes.toVector,
    )

  private def failingInitializationSocket()
      : (NodeSocket, () => Vector[String]) =
    val socket    = js.Dynamic.literal()
    val events    = mutable.ListBuffer.empty[String]
    val listeners = mutable.Map.empty[String, js.Function]
    val register  =
      (
          (event: String, listener: js.Function) =>
            events += s"on:$event"
            listeners.update(event, listener)
            socket,
      ): js.Function2[String, js.Function, js.Dynamic]
    socket.updateDynamic("on")(register)
    socket.updateDynamic("once")(register)
    socket.updateDynamic("off")(
      (
          (event: String, listener: js.Function) =>
            events += s"off:$event"
            listeners
              .get(event)
              .filter(_ == listener)
              .foreach(_ => listeners.remove(event): Unit)
            socket,
      ): js.Function2[String, js.Function, js.Dynamic],
    )
    socket.updateDynamic("write")(
      ((_: String) =>
        events += "write"
        throw IllegalStateException("write failed"),
      ): js.Function1[String, Boolean],
    )
    socket.updateDynamic("destroy")(
      (() =>
        events += "destroy"
        listeners
          .get("close")
          .foreach: listener =>
            events += "emit:close"
            listener.asInstanceOf[js.Function0[Unit]](),
      ): js.Function0[Unit],
    )
    socket.asInstanceOf[NodeSocket] -> (() => events.toVector)

  private def bytes(values: Int*): Uint8Array =
    val result = new Uint8Array(values.length)
    values.zipWithIndex.foreach: (value, index) =>
      result(index) = value.toShort
    result

  private def asciiBytes(value: String): Uint8Array =
    require(
      value.forall(_ <= '\u007f'),
      "ASCII test fixture contained a non-ASCII character",
    )
    bytes(value.iterator.map(_.toInt).toSeq*)

  private def right[A](value: Either[ConfigProblem, A]): A =
    value.fold(problem => fail(problem.message), identity)
