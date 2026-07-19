package org.doctrellis.sbtplugin

import java.io.{ByteArrayOutputStream, IOException, OutputStream}
import java.net.{InetSocketAddress, ServerSocket, URI, UnixDomainSocketAddress}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.{
  Callable,
  CountDownLatch,
  Executor,
  Executors,
  RejectedExecutionException,
  TimeUnit,
}
import java.util.concurrent.atomic.{
  AtomicBoolean,
  AtomicInteger,
  AtomicReference,
}

import scala.jdk.CollectionConverters.*
import scala.util.Using

import sbt.util.LogExchange

class PreviewControlChannelSuite extends munit.FunSuite:
  private val launchId = "test-launch"
  private val hello    =
    """{"version":1,"type":"hello","token":"token","clientId":"client","requestId":"request"}"""

  test("NDJSON framing accepts every split position"):
    val bytes = (hello + "\n").getBytes(StandardCharsets.UTF_8)
    (0 to bytes.length).foreach: split =>
      val decoder = PreviewFrameDecoder(PreviewControlProtocol.MaxFrameBytes)
      val first   = bytes.take(split)
      val second  = bytes.drop(split)
      val frames  = for
        left  <- decoder.offer(first, first.length)
        right <- decoder.offer(second, second.length)
      yield left ++ right
      assertEquals(frames, Right(Vector(hello)), clues(split))

  test("NDJSON framing accepts multiple LF and CRLF frames"):
    val decoder = PreviewFrameDecoder(PreviewControlProtocol.MaxFrameBytes)
    val bytes   = s"$hello\r\n$hello\n".getBytes(StandardCharsets.UTF_8)
    assertEquals(
      decoder.offer(bytes, bytes.length),
      Right(Vector(hello, hello)),
    )

  test("NDJSON framing accepts an exact maximum CRLF frame across chunks"):
    val payload = "x" * PreviewControlProtocol.MaxFrameBytes
    val decoder = PreviewFrameDecoder(PreviewControlProtocol.MaxFrameBytes)
    val prefix  = (payload + "\r").getBytes(StandardCharsets.UTF_8)
    val newline = Array('\n'.toByte)
    assertEquals(decoder.offer(prefix, prefix.length), Right(Vector.empty))
    assertEquals(decoder.offer(newline, newline.length), Right(Vector(payload)))

  test("NDJSON framing rejects malformed and oversized frames"):
    val oversized =
      Array.fill[Byte](PreviewControlProtocol.MaxFrameBytes + 1)('x'.toByte)
    val decoder = PreviewFrameDecoder(PreviewControlProtocol.MaxFrameBytes)
    assert(decoder.offer(oversized, oversized.length).isLeft)
    assert(PreviewControlProtocol.decodeClient("not-json").isLeft)
    assert(
      PreviewControlProtocol
        .decodeClient(hello.replace("\"version\":1", "\"version\":2"))
        .isLeft,
    )

    assert(
      PreviewControlProtocol
        .decodeClient(hello.replace("\"hello\"", "\"unknown\""))
        .isLeft,
    )
    assert(
      PreviewControlProtocol
        .decodeClient(
          hello.replace("\"type\":", "\"type\":\"hello\",\"type\":"),
        )
        .isLeft,
    )

  test("NDJSON framing rejects byte counts outside the input array"):
    val decoder = PreviewFrameDecoder(PreviewControlProtocol.MaxFrameBytes)
    val bytes   = (hello + "\n").getBytes(StandardCharsets.UTF_8)

    assert(decoder.offer(bytes, -1).isLeft)
    assert(decoder.offer(bytes, bytes.length + 1).isLeft)
    assertEquals(decoder.offer(bytes, bytes.length), Right(Vector(hello)))

  test("hello opts into site events only with numeric version one"):
    val optedIn = hello.replace(
      "\"token\":",
      "\"siteEvents\":1,\"token\":",
    )

    assertEquals(
      PreviewControlProtocol.decodeClient(hello),
      Right(
        PreviewControlRequest.Hello(
          token = "token",
          clientId = "client",
          requestId = "request",
          siteEvents = false,
        ),
      ),
    )
    assertEquals(
      PreviewControlProtocol.decodeClient(optedIn),
      Right(
        PreviewControlRequest.Hello(
          token = "token",
          clientId = "client",
          requestId = "request",
          siteEvents = true,
        ),
      ),
    )
    Vector("0", "2", "\"1\"").foreach: value =>
      assertEquals(
        PreviewControlProtocol.decodeClient(
          optedIn.replace("\"siteEvents\":1", s"\"siteEvents\":$value"),
        ),
        PreviewControlProtocol.decodeClient(hello),
        clues(value),
      )
    assertEquals(
      PreviewControlProtocol.decodeClient(
        hello.replace("\"token\":", "\"futureField\":\"value\",\"token\":"),
      ),
      PreviewControlProtocol.decodeClient(hello),
    )

  test("site event activation orders ready before a concurrent publication"):
    val readyEntered       = CountDownLatch(1)
    val releaseReady       = CountDownLatch(1)
    val publicationStarted = CountDownLatch(1)
    val sent   = java.util.concurrent.ConcurrentLinkedQueue[String]()
    val pool   = Executors.newFixedThreadPool(2)
    val events = PreviewConnectionEvents: message =>
      if message == "ready" then
        readyEntered.countDown()
        releaseReady.await()
      sent.add(message)

    try
      val acceptance = pool.submit(
        new Callable[Boolean]:
          override def call(): Boolean =
            events.acceptHello("ready", siteEvents = true),
      )
      assert(readyEntered.await(1L, TimeUnit.SECONDS))
      val publication = pool.submit(
        new Callable[Unit]:
          override def call(): Unit =
            publicationStarted.countDown()
            events.siteChanged("siteChanged"),
      )
      assert(publicationStarted.await(1L, TimeUnit.SECONDS))
      releaseReady.countDown()
      assert(acceptance.get(1L, TimeUnit.SECONDS))
      publication.get(1L, TimeUnit.SECONDS)
      assertEquals(
        sent.iterator().asScala.toVector,
        Vector("ready", "siteChanged"),
      )
    finally
      releaseReady.countDown()
      pool.shutdownNow(): Unit

  test("a writer failure closes its queue and rejects later frames"):
    val failure  = AtomicReference[Throwable]()
    val failed   = CountDownLatch(1)
    val executor = Executors.newSingleThreadExecutor()
    val output   = new OutputStream:
      override def write(value: Int): Unit =
        throw IOException(s"failed byte $value")
    val writer = PreviewFrameWriter(
      output = output,
      executor = executor,
      onFailure = error =>
        failure.set(error)
        failed.countDown()
      ,
      onTerminated = () => (),
    )

    try
      writer.start()
      assert(writer.send("first\n"))
      assert(failed.await(1L, TimeUnit.SECONDS))
      assert(writer.awaitTermination(1L, TimeUnit.SECONDS))
      assertEquals(failure.get().getClass, classOf[IOException])
      assert(!writer.send("second\n"))
    finally
      writer.abort()
      executor.shutdownNow(): Unit

  test("an accepted finish runs once after an earlier frame write fails"):
    val writeEntered         = CountDownLatch(1)
    val releaseWrite         = CountDownLatch(1)
    val writeExited          = AtomicBoolean(false)
    val callbackRan          = CountDownLatch(1)
    val callbackCount        = AtomicInteger(0)
    val callbackSawWriteExit = AtomicBoolean(false)
    val failure              = AtomicReference[Throwable]()
    val failed               = CountDownLatch(1)
    val executor             = Executors.newSingleThreadExecutor()
    val output               = new OutputStream:
      override def write(value: Int): Unit = ()

      override def write(
          bytes: Array[Byte],
          offset: Int,
          length: Int,
      ): Unit =
        writeEntered.countDown()
        if !releaseWrite.await(1L, TimeUnit.SECONDS) then
          throw IOException("controlled write was not released")
        writeExited.set(true)
        throw IOException("controlled write failure")

    val writer = PreviewFrameWriter(
      output = output,
      executor = executor,
      onFailure = error =>
        failure.set(error)
        failed.countDown()
      ,
      onTerminated = () => (),
    )

    try
      writer.start()
      assert(writer.send("frame\n"))
      assert(writeEntered.await(1L, TimeUnit.SECONDS))
      assert(
        writer.finish(() =>
          callbackSawWriteExit.set(writeExited.get())
          callbackCount.incrementAndGet(): Unit
          callbackRan.countDown(),
        ),
      )
      releaseWrite.countDown()

      assert(failed.await(1L, TimeUnit.SECONDS))
      assert(callbackRan.await(1L, TimeUnit.SECONDS))
      assert(writer.awaitTermination(1L, TimeUnit.SECONDS))
      assertEquals(failure.get().getMessage, "controlled write failure")
      assert(callbackSawWriteExit.get())
      assertEquals(callbackCount.get(), 1)

      writer.abort()
      assertEquals(callbackCount.get(), 1)
    finally
      releaseWrite.countDown()
      writer.abort()
      executor.shutdownNow(): Unit

  test("abort preserves an accepted finish while clearing queued frames"):
    val writeEntered         = CountDownLatch(1)
    val releaseWrite         = CountDownLatch(1)
    val writeExited          = AtomicBoolean(false)
    val callbackRan          = CountDownLatch(1)
    val callbackCount        = AtomicInteger(0)
    val callbackSawWriteExit = AtomicBoolean(false)
    val executor             = Executors.newSingleThreadExecutor()
    val output               = new OutputStream:
      override def write(value: Int): Unit = ()

      override def write(
          bytes: Array[Byte],
          offset: Int,
          length: Int,
      ): Unit =
        writeEntered.countDown()
        if !releaseWrite.await(1L, TimeUnit.SECONDS) then
          throw IOException("controlled write was not released")
        writeExited.set(true)

    val writer = PreviewFrameWriter(
      output = output,
      executor = executor,
      onFailure = error => throw error,
      onTerminated = () => (),
    )

    try
      writer.start()
      assert(writer.send("frame\n"))
      assert(writeEntered.await(1L, TimeUnit.SECONDS))
      assert(
        writer.finish(() =>
          callbackSawWriteExit.set(writeExited.get())
          callbackCount.incrementAndGet(): Unit
          callbackRan.countDown(),
        ),
      )

      writer.abort()
      assertEquals(callbackCount.get(), 0)
      releaseWrite.countDown()

      assert(callbackRan.await(1L, TimeUnit.SECONDS))
      assert(writer.awaitTermination(1L, TimeUnit.SECONDS))
      assert(callbackSawWriteExit.get())
      assertEquals(callbackCount.get(), 1)

      writer.abort()
      assertEquals(callbackCount.get(), 1)
    finally
      releaseWrite.countDown()
      writer.abort()
      executor.shutdownNow(): Unit

  test("normal finish invokes its callback after every accepted frame"):
    val output        = ByteArrayOutputStream()
    val callbackRan   = CountDownLatch(1)
    val callbackCount = AtomicInteger(0)
    val observed      = AtomicReference[String]()
    val executor      = Executors.newSingleThreadExecutor()
    val writer        = PreviewFrameWriter(
      output = output,
      executor = executor,
      onFailure = error => throw error,
      onTerminated = () => (),
    )

    try
      writer.start()
      assert(writer.send("first\n"))
      assert(writer.send("second\n"))
      assert(
        writer.finish(() =>
          observed.set(String(output.toByteArray, StandardCharsets.UTF_8))
          callbackCount.incrementAndGet(): Unit
          callbackRan.countDown(),
        ),
      )

      assert(callbackRan.await(1L, TimeUnit.SECONDS))
      assert(writer.awaitTermination(1L, TimeUnit.SECONDS))
      assertEquals(observed.get(), "first\nsecond\n")
      assertEquals(callbackCount.get(), 1)
      assert(!writer.finish(() => callbackCount.incrementAndGet(): Unit))
      assertEquals(callbackCount.get(), 1)
    finally
      writer.abort()
      executor.shutdownNow(): Unit

  test("a finish callback failure still completes the writer exactly once"):
    val callbackCount = AtomicInteger(0)
    val failure       = AtomicReference[Throwable]()
    val failed        = CountDownLatch(1)
    val terminated    = CountDownLatch(1)
    val executor      = Executors.newSingleThreadExecutor()
    val writer        = PreviewFrameWriter(
      output = OutputStream.nullOutputStream(),
      executor = executor,
      onFailure = error =>
        failure.set(error)
        failed.countDown()
      ,
      onTerminated = () => terminated.countDown(),
    )

    try
      writer.start()
      assert(
        writer.finish(() =>
          callbackCount.incrementAndGet(): Unit
          throw IOException("controlled callback failure"),
        ),
      )

      assert(failed.await(1L, TimeUnit.SECONDS))
      assert(terminated.await(1L, TimeUnit.SECONDS))
      assert(writer.awaitTermination(1L, TimeUnit.SECONDS))
      assertEquals(failure.get().getMessage, "controlled callback failure")
      assertEquals(callbackCount.get(), 1)
      writer.abort()
      assertEquals(callbackCount.get(), 1)
    finally
      writer.abort()
      executor.shutdownNow(): Unit

  test("an accepted finish completes when the executor rejects start"):
    val callbackCount = AtomicInteger(0)
    val terminated    = CountDownLatch(1)
    val executor      = new Executor:
      override def execute(command: Runnable): Unit =
        throw RejectedExecutionException("controlled rejection")
    val writer = PreviewFrameWriter(
      output = OutputStream.nullOutputStream(),
      executor = executor,
      onFailure = _ => (),
      onTerminated = () => terminated.countDown(),
    )

    assert(writer.finish(() => callbackCount.incrementAndGet(): Unit))
    val failure = intercept[RejectedExecutionException](writer.start())

    assertEquals(failure.getMessage, "controlled rejection")
    assert(terminated.await(1L, TimeUnit.SECONDS))
    assert(writer.awaitTermination(1L, TimeUnit.SECONDS))
    assertEquals(callbackCount.get(), 1)
    writer.abort()
    assertEquals(callbackCount.get(), 1)

  test("abort before start completes an accepted finish exactly once"):
    val callbackCount = AtomicInteger(0)
    val terminated    = CountDownLatch(1)
    val neverRun      = new Executor:
      override def execute(command: Runnable): Unit =
        fail("the writer must not be started")
    val writer = PreviewFrameWriter(
      output = OutputStream.nullOutputStream(),
      executor = neverRun,
      onFailure = error => throw error,
      onTerminated = () => terminated.countDown(),
    )

    assert(writer.finish(() => callbackCount.incrementAndGet(): Unit))
    writer.abort()

    assert(terminated.await(1L, TimeUnit.SECONDS))
    assert(writer.awaitTermination(1L, TimeUnit.SECONDS))
    assertEquals(callbackCount.get(), 1)
    writer.abort()
    assertEquals(callbackCount.get(), 1)

  test("stale endpoint policy removes only unreachable dead owners"):
    assertEquals(
      StaleEndpointDecision.decide(
        connectSucceeded = false,
        ownerAlive = false,
      ),
      StaleEndpointDecision.RemoveStale,
    )
    assertEquals(
      StaleEndpointDecision.decide(
        connectSucceeded = true,
        ownerAlive = false,
      ),
      StaleEndpointDecision.ProtectLive,
    )

  test("environment contract hashes the canonical workspace, not a subproject"):
    val workspace   = Files.createTempDirectory("doctrellis-workspace-")
    val project     = Files.createDirectories(workspace.resolve("subproject"))
    val runtime     = shortTempDirectory("dtc-config-")
    val canonical   = workspace.toRealPath()
    val hash        = PreviewControlConfig.shortHash(canonical.toString)
    val owner       = Option.unless(isWindows)(runtimeOwner(runtime))
    val environment = Map(
      PreviewControlConfig.ProtocolEnvironment -> PreviewControlConfig.Protocol,
      PreviewControlConfig.RuntimeEnvironment  -> runtime.toString,
      PreviewControlConfig.HashEnvironment     -> hash,
      PreviewControlConfig.WorkspaceEnvironment -> canonical.toString,
      PreviewControlConfig.LaunchEnvironment    -> "environment-launch",
    ) ++ owner.map(value =>
      PreviewControlConfig.OwnerEnvironment -> value.toString,
    )
    try
      assertEquals(
        PreviewControlConfig.fromEnvironment(project, environment),
        Some(
          PreviewControlConfig(
            runtimeDirectory = runtime.toAbsolutePath.normalize(),
            workspaceHash = hash,
            launchId = "environment-launch",
            runtimeOwner = owner,
          ),
        ),
      )
      assert(
        intercept[IllegalArgumentException](
          PreviewControlConfig.fromEnvironment(
            project,
            environment.updated(
              PreviewControlConfig.HashEnvironment,
              "0" * 24,
            ),
          ),
        ).getMessage.contains("hash"),
      )
      assert(
        intercept[IllegalArgumentException](
          PreviewControlConfig.fromEnvironment(
            project,
            environment - PreviewControlConfig.LaunchEnvironment,
          ),
        ).getMessage.contains("launch ID"),
      )
    finally
      deleteTree(runtime)
      deleteTree(workspace)

  test("stale socket cleanup requires a parsed dead owner"):
    val runtime      = shortTempDirectory("dtc-stale-")
    val hash         = "1123456789abcdef01234567"
    val socket       = runtime.resolve(s"$hash.0123456789abcdef.sock")
    val metadataPath = runtime.resolve(s"$hash.json")
    val stale        = PreviewControlMetadata(
      transport = PreviewControlTransport.Unix(socket),
      pid = Long.MaxValue,
      sessionId = "stale-session",
      launchId = "stale-launch",
      token = "0" * 64,
    )
    assert(
      PreviewControlMetadata
        .parse(stale.json.replace("0" * 64, "short"))
        .isLeft,
    )
    try
      Files.writeString(socket, "stale")
      Files.writeString(metadataPath, stale.json)
      val prepared = PreviewControlEndpoint.prepare(
        config = PreviewControlConfig(
          runtimeDirectory = runtime,
          workspaceHash = hash,
          launchId = launchId,
        ),
        pid = ProcessHandle.current().pid(),
        sessionId = "replacement-session",
        token = "1" * 64,
      )
      assert(!Files.exists(socket))
      assertEquals(
        right(
          PreviewControlMetadata.parse(Files.readString(metadataPath)),
        ).sessionId,
        "replacement-session",
      )
      PreviewControlEndpoint.cleanupOwned(
        metadataPath = prepared.metadataPath,
        socketPath = prepared.socketPath,
        sessionId = "replacement-session",
      )
      prepared.lease.close()
    finally deleteTree(runtime)

  test("metadata parsing rejects invalid Unix paths without throwing"):
    val absolute = Path
      .of(System.getProperty("java.io.tmpdir"))
      .toAbsolutePath
      .normalize()
      .resolve("doctrellis-metadata.sock")

    assert(
      PreviewControlMetadata.parse(unixMetadataJson(absolute.toString)).isRight,
    )
    Vector("", "relative.sock", "\u0000").foreach: address =>
      assert(
        PreviewControlMetadata.parse(unixMetadataJson(address)).isLeft,
        clues(address.toVector.map(_.toInt)),
      )

  test("metadata parsing accepts only connectable TCP ports"):
    def metadata(port: Int): String =
      PreviewControlMetadata(
        transport = PreviewControlTransport.Tcp("127.0.0.1", port),
        pid = ProcessHandle.current().pid(),
        sessionId = "metadata-session",
        launchId = "metadata-launch",
        token = "0" * 64,
      ).json

    assert(PreviewControlMetadata.parse(metadata(1)).isRight)
    assert(PreviewControlMetadata.parse(metadata(65535)).isRight)
    assert(PreviewControlMetadata.parse(metadata(0)).isLeft)
    assert(PreviewControlMetadata.parse(metadata(65536)).isLeft)

  test("Windows binding metadata recovers after a crash and open failure"):
    val runtime = shortTempDirectory("dtc-windows-provisional-")
    val hash    = "8123456789abcdef01234567"
    val config  = PreviewControlConfig(
      runtimeDirectory = runtime,
      workspaceHash = hash,
      launchId = launchId,
    )
    var crashed     = Option.empty[PreviewControlPaths]
    var replacement = Option.empty[PreviewControlPaths]
    try
      crashed = Some(
        PreviewControlEndpoint.prepare(
          config = config,
          pid = Long.MaxValue,
          sessionId = "crashed-windows-session",
          token = "0" * 64,
          windows = true,
        ),
      )
      val metadataPath = runtime.resolve(s"$hash.json")
      val provisional  = Files.readString(metadataPath)
      assert(PreviewControlMetadata.parse(provisional).isLeft)
      assertEquals(
        right(PreviewControlMetadata.parseProvisional(provisional)).transport,
        PreviewControlTransport.Tcp("127.0.0.1", 0),
      )

      crashed.foreach(_.lease.close())
      replacement = Some(
        PreviewControlEndpoint.prepare(
          config = config,
          pid = ProcessHandle.current().pid(),
          sessionId = "replacement-windows-session",
          token = "1" * 64,
          windows = true,
        ),
      )

      PreviewControlEndpoint.cleanupOwned(
        metadataPath = metadataPath,
        socketPath = None,
        sessionId = "different-windows-session",
      )
      assert(Files.exists(metadataPath))
      PreviewControlEndpoint.cleanupOwned(
        metadataPath = metadataPath,
        socketPath = None,
        sessionId = "replacement-windows-session",
      )
      assert(Files.notExists(metadataPath))
    finally
      crashed.foreach(_.lease.close())
      replacement.foreach(_.lease.close())
      deleteTree(runtime)

  test("unmarked port-zero metadata remains protected"):
    val runtime      = shortTempDirectory("dtc-windows-protect-")
    val hash         = "9123456789abcdef01234567"
    val metadataPath = runtime.resolve(s"$hash.json")
    val unmarked     = PreviewControlMetadata(
      transport = PreviewControlTransport.Tcp("127.0.0.1", 0),
      pid = Long.MaxValue,
      sessionId = "unmarked-session",
      launchId = "unmarked-launch",
      token = "0" * 64,
    ).json
    try
      Files.writeString(metadataPath, unmarked)
      intercept[IllegalStateException](
        PreviewControlEndpoint.prepare(
          config = PreviewControlConfig(
            runtimeDirectory = runtime,
            workspaceHash = hash,
            launchId = launchId,
          ),
          pid = ProcessHandle.current().pid(),
          sessionId = "replacement-session",
          token = "1" * 64,
          windows = true,
        ),
      ): Unit
      assertEquals(Files.readString(metadataPath), unmarked)
    finally deleteTree(runtime)

  test("an endpoint lease serializes simultaneous stale cleanup"):
    val runtime      = shortTempDirectory("dtc-race-")
    val hash         = "3123456789abcdef01234567"
    val staleSocket  = runtime.resolve(s"$hash.0123456789abcdef.sock")
    val metadataPath = runtime.resolve(s"$hash.json")
    val stale        = PreviewControlMetadata(
      transport = PreviewControlTransport.Unix(staleSocket),
      pid = Long.MaxValue,
      sessionId = "stale-session",
      launchId = "stale-launch",
      token = "0" * 64,
    )
    val start = CountDownLatch(1)
    val pool  = Executors.newFixedThreadPool(2)
    try
      Files.writeString(staleSocket, "stale")
      Files.writeString(metadataPath, stale.json)
      val attempts = Vector("first-session", "second-session").map: session =>
        pool.submit(
          new Callable[Either[Throwable, PreviewControlPaths]]:
            override def call(): Either[Throwable, PreviewControlPaths] =
              start.await()
              scala.util
                .Try(
                  PreviewControlEndpoint.prepare(
                    config = PreviewControlConfig(
                      runtimeDirectory = runtime,
                      workspaceHash = hash,
                      launchId = launchId,
                    ),
                    pid = ProcessHandle.current().pid(),
                    sessionId = session,
                    token = "1" * 64,
                  ),
                )
                .toEither,
        )
      start.countDown()
      val results = attempts.map(_.get(5L, TimeUnit.SECONDS))
      val winners = results.collect { case Right(paths) => paths }
      assertEquals(winners.size, 1)
      val winner = winners match
        case Seq(only) => only
        case other     => fail(s"expected one endpoint winner, got $other")
      val metadata = right(
        PreviewControlMetadata.parse(Files.readString(metadataPath)),
      )
      assert(
        Set("first-session", "second-session").contains(metadata.sessionId),
      )
      assert(winner.socketPath.forall(Files.notExists(_)))
      PreviewControlEndpoint.cleanupOwned(
        metadataPath = winner.metadataPath,
        socketPath = winner.socketPath,
        sessionId = metadata.sessionId,
      )
      winner.lease.close()
    finally
      pool.shutdownNow()
      deleteTree(runtime)

  test("runtime validation rejects symlinks and a different owner"):
    if isWindows then ()
    else
      val parent = shortTempDirectory("dtc-owner-")
      val target = Files.createDirectory(parent.resolve("target"))
      val link   = parent.resolve("link")
      try
        Files.createSymbolicLink(link, target)
        intercept[IllegalStateException](
          PreviewControlEndpoint.prepare(
            config = PreviewControlConfig(
              runtimeDirectory = link,
              workspaceHash = "4123456789abcdef01234567",
              launchId = launchId,
              runtimeOwner = Some(runtimeOwner(target)),
            ),
            pid = ProcessHandle.current().pid(),
            sessionId = "symlink-session",
            token = "1" * 64,
          ),
        ): Unit
        intercept[IllegalStateException](
          PreviewControlEndpoint.prepare(
            config = PreviewControlConfig(
              runtimeDirectory = target,
              workspaceHash = "5123456789abcdef01234567",
              launchId = launchId,
              runtimeOwner = Some(runtimeOwner(target) + 1L),
            ),
            pid = ProcessHandle.current().pid(),
            sessionId = "owner-session",
            token = "1" * 64,
          ),
        ): Unit
      finally deleteTree(parent)

  test("malformed ownership metadata is protected instead of unlinked"):
    val runtime      = shortTempDirectory("dtc-protect-")
    val hash         = "2123456789abcdef01234567"
    val metadataPath = runtime.resolve(s"$hash.json")
    try
      Files.writeString(metadataPath, "not-json")
      intercept[IllegalStateException](
        PreviewControlEndpoint.prepare(
          config = PreviewControlConfig(
            runtimeDirectory = runtime,
            workspaceHash = hash,
            launchId = launchId,
          ),
          pid = ProcessHandle.current().pid(),
          sessionId = "new-session",
          token = "1" * 64,
        ),
      ): Unit
      assertEquals(Files.readString(metadataPath), "not-json")
    finally deleteTree(runtime)
    assertEquals(
      StaleEndpointDecision.decide(
        connectSucceeded = false,
        ownerAlive = true,
      ),
      StaleEndpointDecision.ProtectLive,
    )

  test("two port-zero HTTP servers bind distinct positive ports"):
    withSite: site =>
      val first  = PreviewHttpBinding.start(site, "127.0.0.1", 0)
      val second = PreviewHttpBinding.start(site, "127.0.0.1", 0)
      try
        assert(first.port > 0)
        assert(second.port > 0)
        assertNotEquals(first.port, second.port)
        assertEquals(status(first.url), 200)
        assertEquals(status(second.url), 200)
      finally
        first.stopServer()
        first.stopExecutor()
        second.stopServer()
        second.stopExecutor()

  test("a configured positive port is preserved for direct CLI previews"):
    withSite: site =>
      val configured = Using.resource(new ServerSocket(0))(_.getLocalPort)
      val binding    = PreviewHttpBinding.start(
        site,
        "127.0.0.1",
        configured,
      )
      try
        assertEquals(binding.port, configured)
        assertEquals(status(binding.url), 200)
      finally
        binding.stopServer()
        binding.stopExecutor()
      assertPortReusable(configured)

  test("site changes reach every opted-in connection and skip legacy clients"):
    val runtime = shortTempDirectory("dtc-events-")
    val hash    = "6123456789abcdef01234567"
    val control = PreviewControlServer.open(
      config = PreviewControlConfig(
        runtimeDirectory = runtime,
        workspaceHash = hash,
        launchId = launchId,
      ),
      readyUrl = "http://127.0.0.1:4242",
      onStop = () => (),
      log = LogExchange.logger("preview-control-events-test"),
    )
    val metadata = right(
      PreviewControlMetadata.parse(
        Files.readString(runtime.resolve(s"$hash.json")),
      ),
    )
    val owner    = connect(metadata)
    val observer = connect(metadata)
    val legacy   = connect(metadata)

    try
      Vector(
        (owner, "owner", true),
        (observer, "observer", true),
        (legacy, "legacy", false),
      ).foreach: (client, clientId, siteEvents) =>
        write(
          client,
          request(
            kind = "hello",
            token = metadata.token,
            requestId = s"hello-$clientId",
            clientId = clientId,
            siteEvents = siteEvents,
          ),
        )
        assert(read(client).contains("\"type\":\"ready\""))

      control.siteChanged(2L)
      assertSiteChanged(read(owner), metadata.sessionId, 2L)
      assertSiteChanged(read(observer), metadata.sessionId, 2L)

      write(
        legacy,
        request(
          kind = "ping",
          token = metadata.token,
          requestId = "legacy-ping",
          clientId = "legacy",
        ),
      )
      assertEquals(
        right(PreviewControlJson.parse(read(legacy))).get("type"),
        Some(PreviewControlJson.Atom.Text("pong")),
      )

      observer.close()
      control.siteChanged(3L)
      assertSiteChanged(read(owner), metadata.sessionId, 3L)

      control.close()
      assert(Files.notExists(runtime.resolve(s"$hash.json")))
      metadata.transport match
        case PreviewControlTransport.Unix(address) =>
          assert(Files.notExists(address))
        case PreviewControlTransport.Tcp(_, _) => ()
      assertChannelClosed(owner)
      assertChannelClosed(legacy)
      control.siteChanged(4L)
    finally
      owner.close()
      observer.close()
      legacy.close()
      control.close()
      deleteTree(runtime)

  test("concurrent responses and site events preserve complete NDJSON frames"):
    val runtime    = shortTempDirectory("dtc-framing-")
    val hash       = "7123456789abcdef01234567"
    val frameCount = 64
    val control    = PreviewControlServer.open(
      config = PreviewControlConfig(
        runtimeDirectory = runtime,
        workspaceHash = hash,
        launchId = launchId,
      ),
      readyUrl = "http://127.0.0.1:4242",
      onStop = () => (),
      log = LogExchange.logger("preview-control-framing-test"),
    )
    val metadata = right(
      PreviewControlMetadata.parse(
        Files.readString(runtime.resolve(s"$hash.json")),
      ),
    )
    val client = connect(metadata)
    val start  = CountDownLatch(1)
    val pool   = Executors.newFixedThreadPool(2)

    try
      write(
        client,
        request(
          kind = "hello",
          token = metadata.token,
          requestId = "hello-framing",
          clientId = "framing-client",
          siteEvents = true,
        ),
      )
      assert(read(client).contains("\"type\":\"ready\""))

      val pings = pool.submit(
        new Callable[Unit]:
          override def call(): Unit =
            start.await()
            (1 to frameCount).foreach: index =>
              write(
                client,
                request(
                  kind = "ping",
                  token = metadata.token,
                  requestId = s"ping-$index",
                  clientId = "framing-client",
                ),
              ),
      )
      val events = pool.submit(
        new Callable[Unit]:
          override def call(): Unit =
            start.await()
            (1 to frameCount).foreach(index => control.siteChanged(index + 1L)),
      )
      start.countDown()
      pings.get(2L, TimeUnit.SECONDS)
      events.get(2L, TimeUnit.SECONDS)

      val frames = Vector.fill(frameCount * 2)(
        right(PreviewControlJson.parse(read(client))),
      )
      val kinds = frames.flatMap(_.get("type"))
      assertEquals(
        kinds.count(_ == PreviewControlJson.Atom.Text("pong")),
        frameCount,
      )
      assertEquals(
        kinds.count(_ == PreviewControlJson.Atom.Text("siteChanged")),
        frameCount,
      )
      val revisions = frames.flatMap: fields =>
        fields
          .get("revision")
          .collect:
            case PreviewControlJson.Atom.Number(value) => value
      assertEquals(revisions, (2L to (frameCount + 1L)).toVector)
    finally
      client.close()
      control.close()
      pool.shutdownNow()
      deleteTree(runtime)

  test("control channel authenticates, announces actual URL, and stops HTTP"):
    withSite: site =>
      val runtime  = shortTempDirectory("dtc-")
      val binding  = PreviewHttpBinding.start(site, "127.0.0.1", 0)
      val shutdown = PreviewServerShutdown(
        stopServer = () => binding.stopServer(),
        stopExecutor = () => binding.stopExecutor(),
        signalStopped = () => (),
      )
      val control = PreviewControlServer.open(
        config = PreviewControlConfig(
          runtimeDirectory = runtime,
          workspaceHash = "0123456789abcdef01234567",
          launchId = launchId,
        ),
        readyUrl = binding.url,
        onStop = () => shutdown.stop(),
        log = LogExchange.logger("preview-control-test"),
      )
      try
        assert(
          control.awaitStopAcknowledgedIfRequested(0L, TimeUnit.NANOSECONDS),
        )
        val metadataPath = runtime.resolve("0123456789abcdef01234567.json")
        val metadata     = right(
          PreviewControlMetadata.parse(Files.readString(metadataPath)),
        )
        intercept[IllegalStateException](
          PreviewControlServer.open(
            config = PreviewControlConfig(
              runtimeDirectory = runtime,
              workspaceHash = "0123456789abcdef01234567",
              launchId = "competing-launch",
            ),
            readyUrl = binding.url,
            onStop = () => (),
            log = LogExchange.logger("preview-control-conflict-test"),
          ),
        ): Unit
        assertEquals(
          right(
            PreviewControlMetadata.parse(Files.readString(metadataPath)),
          ).sessionId,
          metadata.sessionId,
        )
        val rejected = connect(metadata)
        try
          write(
            rejected,
            request(
              kind = "hello",
              token = "0" * 64,
              requestId = "wrong-token",
            ),
          )
          assert(read(rejected).contains("authenticationFailed"))
        finally rejected.close()

        val conflictingClient = connect(metadata)
        try
          write(
            conflictingClient,
            request(
              kind = "hello",
              token = metadata.token,
              requestId = "hello-conflict",
            ),
          )
          assert(read(conflictingClient).contains("\"type\":\"ready\""))
          write(
            conflictingClient,
            request(
              kind = "ping",
              token = metadata.token,
              requestId = "ping-conflict",
            ).replace("suite-client", "other-client"),
          )
          assert(read(conflictingClient).contains("authenticationFailed"))
        finally conflictingClient.close()

        val client = connect(metadata)
        try
          write(
            client,
            request(
              kind = "hello",
              token = metadata.token,
              requestId = "hello-1",
            ),
          )
          val ready = read(client)
          assert(ready.contains("\"type\":\"ready\""))
          assert(ready.contains(PreviewControlJson.quoted(binding.url)))
          assert(ready.contains(s"\"pid\":${ProcessHandle.current().pid()}"))
          assert(ready.contains(s"\"launchId\":\"$launchId\""))
          assertEquals(status(binding.url), 200)

          write(
            client,
            request(
              kind = "ping",
              token = metadata.token,
              requestId = "ping-1",
            ),
          )
          assert(read(client).contains("\"type\":\"pong\""))

          write(
            client,
            request(
              kind = "stop",
              token = metadata.token,
              requestId = "stop-1",
            ),
          )
          assert(read(client).contains("\"type\":\"stopping\""))
          assert(read(client).contains("\"type\":\"stopped\""))
        finally client.close()

        assert(
          control.awaitStopAcknowledgedIfRequested(1L, TimeUnit.SECONDS),
        )
        assertHttpStopped(binding.url)
        assertPortReusable(binding.port)
      finally
        shutdown.stop()
        control.close()
        deleteTree(runtime)

  private def request(
      kind: String,
      token: String,
      requestId: String,
      clientId: String = "suite-client",
      siteEvents: Boolean = false,
  ): String =
    PreviewControlJson.objectValue(
      Vector(
        "version"   -> "1",
        "type"      -> PreviewControlJson.quoted(kind),
        "token"     -> PreviewControlJson.quoted(token),
        "clientId"  -> PreviewControlJson.quoted(clientId),
        "requestId" -> PreviewControlJson.quoted(requestId),
      ) ++ Option
        .when(kind == "hello" && siteEvents)("siteEvents" -> "1")
        .toVector,
    ) + "\n"

  private def assertSiteChanged(
      frame: String,
      sessionId: String,
      revision: Long,
  ): Unit =
    assertEquals(
      PreviewControlJson.parse(frame),
      Right(
        Map(
          "version"   -> PreviewControlJson.Atom.Number(1L),
          "type"      -> PreviewControlJson.Atom.Text("siteChanged"),
          "sessionId" -> PreviewControlJson.Atom.Text(sessionId),
          "revision"  -> PreviewControlJson.Atom.Number(revision),
        ),
      ),
    )

  private def assertChannelClosed(channel: SocketChannel): Unit =
    val executor = Executors.newSingleThreadExecutor()
    try
      val result = executor.submit(
        new Callable[Int]:
          override def call(): Int = channel.read(ByteBuffer.allocate(1)),
      )
      assertEquals(result.get(1L, TimeUnit.SECONDS), -1)
    finally executor.shutdownNow(): Unit

  private def unixMetadataJson(address: String): String =
    PreviewControlJson.objectValue(
      Vector(
        "version"   -> "1",
        "protocol"  -> PreviewControlJson.quoted(PreviewControlConfig.Protocol),
        "pid"       -> ProcessHandle.current().pid().toString,
        "sessionId" -> PreviewControlJson.quoted("metadata-session"),
        "launchId"  -> PreviewControlJson.quoted("metadata-launch"),
        "token"     -> PreviewControlJson.quoted("0" * 64),
        "transport" -> PreviewControlJson.quoted("unix"),
        "address"   -> PreviewControlJson.quoted(address),
      ),
    )

  private def connect(metadata: PreviewControlMetadata): SocketChannel =
    metadata.transport match
      case PreviewControlTransport.Unix(address) =>
        SocketChannel.open(UnixDomainSocketAddress.of(address))
      case PreviewControlTransport.Tcp(host, port) =>
        SocketChannel.open(InetSocketAddress(host, port))

  private def write(channel: SocketChannel, value: String): Unit =
    val bytes = ByteBuffer.wrap(value.getBytes(StandardCharsets.UTF_8))
    while bytes.hasRemaining do channel.write(bytes): Unit

  private def read(channel: SocketChannel): String =
    val result = StringBuilder()
    val byte   = ByteBuffer.allocate(1)
    var done   = false
    while !done do
      byte.clear()
      val count = channel.read(byte)
      if count < 0 then fail("IPC channel ended before a complete frame")
      else if count > 0 then
        byte.flip()
        val value = byte.get()
        if value == '\n'.toByte then done = true
        else if value != '\r'.toByte then result.append(value.toChar)
    result.toString

  private def status(url: String): Int =
    HttpClient
      .newHttpClient()
      .send(
        HttpRequest.newBuilder(URI.create(url)).GET().build(),
        HttpResponse.BodyHandlers.discarding(),
      )
      .statusCode()

  private def assertHttpStopped(url: String): Unit =
    intercept[java.io.IOException](status(url)): Unit

  private def assertPortReusable(port: Int): Unit =
    Using.resource(new ServerSocket()): socket =>
      socket.setReuseAddress(true)
      socket.bind(InetSocketAddress("127.0.0.1", port))

  private def withSite(action: Path => Unit): Unit =
    val site = Files.createTempDirectory("doctrellis-site-")
    try
      Files.writeString(site.resolve("index.html"), "<h1>ready</h1>")
      action(site)
    finally deleteTree(site)

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      Using.resource(Files.walk(root)): paths =>
        paths.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)

  private def runtimeOwner(path: Path): Long =
    Files.getAttribute(path, "unix:uid") match
      case number: Number => number.longValue()
      case other          =>
        fail(s"expected unix:uid to be numeric, got ${String.valueOf(other)}")

  private def shortTempDirectory(prefix: String): Path =
    if isWindows then Files.createTempDirectory(prefix)
    else Files.createTempDirectory(Path.of("/tmp"), prefix)

  private def isWindows: Boolean =
    System
      .getProperty("os.name", "")
      .toLowerCase(java.util.Locale.ROOT)
      .contains("win")

  private def right[A](value: Either[String, A]): A =
    value.fold(fail(_), identity)
