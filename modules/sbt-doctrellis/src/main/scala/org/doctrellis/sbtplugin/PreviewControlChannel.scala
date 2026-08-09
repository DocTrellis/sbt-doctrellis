package org.doctrellis.sbtplugin

import java.io.ByteArrayOutputStream
import java.net.{
  InetSocketAddress,
  StandardProtocolFamily,
  UnixDomainSocketAddress,
}
import java.nio.ByteBuffer
import java.nio.channels.{
  Channels,
  FileChannel,
  FileLock,
  OverlappingFileLockException,
  ServerSocketChannel,
  SocketChannel,
}
import java.nio.charset.{
  CharacterCodingException,
  CodingErrorAction,
  StandardCharsets,
}
import java.nio.file.{
  Files,
  InvalidPathException,
  LinkOption,
  Path,
  StandardCopyOption,
  StandardOpenOption,
}
import java.nio.file.attribute.{BasicFileAttributes, PosixFilePermissions}
import java.security.{MessageDigest, SecureRandom}
import java.util.UUID
import java.util.concurrent.{
  CountDownLatch,
  Executor,
  Executors,
  LinkedBlockingQueue,
  TimeUnit,
}
import java.util.concurrent.atomic.AtomicBoolean

import scala.annotation.tailrec
import scala.util.control.NonFatal

import sbt.Logger

private[sbtplugin] final case class PreviewControlConfig(
    runtimeDirectory: Path,
    workspaceHash: String,
    launchId: String,
    runtimeOwner: Option[Long] = None,
)

private[sbtplugin] enum PreviewControlConfigProblem:
  case UnsupportedProtocol(actual: String)
  case Missing(field: String)
  case Invalid(field: String, detail: String)
  case CannotResolve(field: String, value: String, detail: String)
  case WorkingDirectoryOutsideWorkspace
  case WorkspaceHashMismatch
  case RuntimeDirectoryInsideWorkspace

  def message: String = this match
    case UnsupportedProtocol(actual) =>
      s"Unsupported DocTrellis preview protocol '$actual'."
    case Missing(field)                      => s"Missing $field."
    case Invalid(field, detail)              => s"The $field $detail."
    case CannotResolve(field, value, detail) =>
      s"Could not resolve the $field '$value': $detail"
    case WorkingDirectoryOutsideWorkspace =>
      "The preview working directory is outside the canonical workspace."
    case WorkspaceHashMismatch =>
      "The workspace IPC hash does not match the canonical workspace path."
    case RuntimeDirectoryInsideWorkspace =>
      "The workspace IPC endpoint must not be stored inside the project."

private[sbtplugin] final case class PreviewControlEnvironment private (
    canonicalWorkspace: String,
    runtimeDirectory: String,
    workspaceHash: String,
    launchId: String,
    runtimeOwner: Option[Long],
)

private[sbtplugin] object PreviewControlEnvironment:
  def parse(
      environment: Map[String, String],
      runtimeOwnerRequired: Boolean,
  ): Either[PreviewControlConfigProblem, Option[PreviewControlEnvironment]] =
    environment.get(PreviewControlConfig.ProtocolEnvironment) match
      case None           => Right(None)
      case Some(protocol) =>
        for
          _ <- Either.cond(
            protocol == PreviewControlConfig.Protocol,
            (),
            PreviewControlConfigProblem.UnsupportedProtocol(protocol),
          )
          canonical <- required(
            environment,
            PreviewControlConfig.WorkspaceEnvironment,
            "canonical workspace path",
          )
          runtime <- required(
            environment,
            PreviewControlConfig.RuntimeEnvironment,
            "workspace IPC runtime directory",
          )
          hash <- required(
            environment,
            PreviewControlConfig.HashEnvironment,
            "workspace IPC hash",
          )
          launchId <- required(
            environment,
            PreviewControlConfig.LaunchEnvironment,
            "workspace launch ID",
          )
          owner <- runtimeOwner(
            environment.get(PreviewControlConfig.OwnerEnvironment),
            runtimeOwnerRequired,
          )
        yield Some(
          PreviewControlEnvironment(
            canonicalWorkspace = canonical,
            runtimeDirectory = runtime,
            workspaceHash = hash,
            launchId = launchId,
            runtimeOwner = owner,
          ),
        )

  private def required(
      environment: Map[String, String],
      name: String,
      field: String,
  ): Either[PreviewControlConfigProblem, String] =
    environment
      .get(name)
      .filter(_.nonEmpty)
      .toRight(PreviewControlConfigProblem.Missing(field))

  private def runtimeOwner(
      value: Option[String],
      required: Boolean,
  ): Either[PreviewControlConfigProblem, Option[Long]] = value match
    case Some(raw) =>
      raw.toLongOption
        .filter(_ >= 0L)
        .map(Some(_))
        .toRight(
          PreviewControlConfigProblem.Invalid(
            "workspace IPC runtime owner",
            "must be a non-negative integer",
          ),
        )
    case None if required =>
      Left(
        PreviewControlConfigProblem.Missing(
          "workspace IPC runtime owner",
        ),
      )
    case None => Right(None)

private[sbtplugin] object PreviewControlConfig:
  val Protocol             = "doctrellis-preview-v1"
  val ProtocolEnvironment  = "DOCTRELLIS_PREVIEW_PROTOCOL"
  val RuntimeEnvironment   = "DOCTRELLIS_RUNTIME_DIR"
  val HashEnvironment      = "DOCTRELLIS_WORKSPACE_HASH"
  val WorkspaceEnvironment = "DOCTRELLIS_WORKSPACE_CANONICAL"
  val OwnerEnvironment     = "DOCTRELLIS_RUNTIME_OWNER"
  val LaunchEnvironment    = "DOCTRELLIS_LAUNCH_ID"

  def fromEnvironment(
      workspace: Path,
      environment: Map[String, String],
      runtimeOwnerRequired: Boolean,
  ): Either[PreviewControlConfigProblem, Option[PreviewControlConfig]] =
    PreviewControlEnvironment
      .parse(environment, runtimeOwnerRequired)
      .flatMap:
        case Some(value) => resolve(workspace, value).map(Some(_))
        case None        => Right(None)

  def resolve(
      workspace: Path,
      environment: PreviewControlEnvironment,
  ): Either[PreviewControlConfigProblem, PreviewControlConfig] =
    for
      project   <- canonicalPath("preview working directory", workspace)
      canonical <- canonicalPath(
        "canonical workspace path",
        environment.canonicalWorkspace,
      )
      _ <- Either.cond(
        project.startsWith(canonical),
        (),
        PreviewControlConfigProblem.WorkingDirectoryOutsideWorkspace,
      )
      expected = shortHash(environment.canonicalWorkspace)
      _ <- Either.cond(
        environment.workspaceHash == expected,
        (),
        PreviewControlConfigProblem.WorkspaceHashMismatch,
      )
      runtime <- canonicalPath(
        "workspace IPC runtime directory",
        environment.runtimeDirectory,
      )
      _ <- Either.cond(
        !runtime.startsWith(canonical),
        (),
        PreviewControlConfigProblem.RuntimeDirectoryInsideWorkspace,
      )
    yield PreviewControlConfig(
      runtimeDirectory = runtime,
      workspaceHash = expected,
      launchId = environment.launchId,
      runtimeOwner = environment.runtimeOwner,
    )

  def shortHash(value: String): String =
    MessageDigest
      .getInstance("SHA-256")
      .digest(value.getBytes(StandardCharsets.UTF_8))
      .iterator
      .map(byte => f"${byte & 0xff}%02x")
      .mkString
      .take(24)

  def runtimeOwnerRequired: Boolean =
    !isWindows

  private def canonicalPath(
      field: String,
      value: Path,
  ): Either[PreviewControlConfigProblem, Path] =
    try Right(value.toRealPath())
    catch
      case NonFatal(error) =>
        Left(
          PreviewControlConfigProblem.CannotResolve(
            field,
            value.toString,
            errorMessage(error),
          ),
        )

  private def canonicalPath(
      field: String,
      value: String,
  ): Either[PreviewControlConfigProblem, Path] =
    try Right(Path.of(value).toRealPath())
    catch
      case NonFatal(error) =>
        Left(
          PreviewControlConfigProblem.CannotResolve(
            field,
            value,
            errorMessage(error),
          ),
        )

  private def errorMessage(error: Throwable): String =
    error.toString

  private def isWindows: Boolean =
    System
      .getProperty("os.name", "")
      .toLowerCase(java.util.Locale.ROOT)
      .contains("win")

private[sbtplugin] enum PreviewControlTransport:
  case Unix(address: Path)
  case Tcp(host: String, port: Int)

private[sbtplugin] final case class PreviewControlMetadata(
    transport: PreviewControlTransport,
    pid: Long,
    sessionId: String,
    launchId: String,
    token: String,
):
  def json: String = PreviewControlMetadata.json(this, Vector.empty)

private[sbtplugin] object PreviewControlMetadata:
  private val ProvisionalState  = "binding"
  private val ProvisionalFields = Set(
    "version",
    "protocol",
    "pid",
    "sessionId",
    "launchId",
    "token",
    "transport",
    "host",
    "port",
    "state",
  )

  def parse(value: String): Either[String, PreviewControlMetadata] =
    PreviewControlJson
      .parse(value)
      .flatMap(fields => parseFields(fields, port => port > 0 && port <= 65535))

  def provisionalJson(metadata: PreviewControlMetadata): String =
    json(
      metadata,
      Vector("state" -> PreviewControlJson.quoted(ProvisionalState)),
    )

  def parseProvisional(
      value: String,
  ): Either[String, PreviewControlMetadata] =
    PreviewControlJson
      .parse(value)
      .flatMap: fields =>
        for
          _ <- check(
            fields.keySet == ProvisionalFields,
            "metadata is not a DocTrellis TCP binding record",
          )
          state <- string(fields, "state")
          _     <- check(
            state == ProvisionalState,
            "metadata is not a DocTrellis TCP binding record",
          )
          metadata <- parseFields(fields, _ == 0)
        yield metadata

  private def json(
      metadata: PreviewControlMetadata,
      extraFields: Vector[(String, String)],
  ): String =
    val common = Vector(
      "version"   -> "1",
      "protocol"  -> PreviewControlJson.quoted(PreviewControlConfig.Protocol),
      "pid"       -> metadata.pid.toString,
      "sessionId" -> PreviewControlJson.quoted(metadata.sessionId),
      "launchId"  -> PreviewControlJson.quoted(metadata.launchId),
      "token"     -> PreviewControlJson.quoted(metadata.token),
    )
    val transportFields = metadata.transport match
      case PreviewControlTransport.Unix(address) =>
        Vector(
          "transport" -> PreviewControlJson.quoted("unix"),
          "address"   -> PreviewControlJson.quoted(address.toString),
        )
      case PreviewControlTransport.Tcp(host, port) =>
        Vector(
          "transport" -> PreviewControlJson.quoted("tcp"),
          "host"      -> PreviewControlJson.quoted(host),
          "port"      -> port.toString,
        )
    PreviewControlJson.objectValue(common ++ transportFields ++ extraFields)

  private def parseFields(
      fields: Map[String, PreviewControlJson.Atom],
      validTcpPort: Int => Boolean,
  ): Either[String, PreviewControlMetadata] =
    for
      version  <- integer(fields, "version")
      _        <- check(version == 1, s"unsupported metadata version $version")
      protocol <- string(fields, "protocol")
      _        <- check(
        protocol == PreviewControlConfig.Protocol,
        s"unsupported metadata protocol '$protocol'",
      )
      pid       <- long(fields, "pid")
      _         <- check(pid > 0L, "metadata pid must be positive")
      sessionId <- string(fields, "sessionId")
      _         <- check(sessionId.nonEmpty, "metadata sessionId is empty")
      launchId  <- string(fields, "launchId")
      _         <- check(launchId.nonEmpty, "metadata launchId is empty")
      token     <- string(fields, "token")
      _         <- check(token.length >= 32, "metadata token is too short")
      kind      <- string(fields, "transport")
      transport <- kind match
        case "unix" =>
          string(fields, "address")
            .flatMap(unixAddress)
            .map(PreviewControlTransport.Unix(_))
        case "tcp" =>
          for
            host <- string(fields, "host")
            port <- integer(fields, "port")
            _    <- check(
              host == "127.0.0.1",
              "metadata TCP host is not loopback",
            )
            _ <- check(
              validTcpPort(port),
              "metadata TCP port is invalid",
            )
          yield PreviewControlTransport.Tcp(host, port)
        case other => Left(s"unknown metadata transport '$other'")
    yield PreviewControlMetadata(
      transport = transport,
      pid = pid,
      sessionId = sessionId,
      launchId = launchId,
      token = token,
    )

  private def string(
      fields: Map[String, PreviewControlJson.Atom],
      name: String,
  ): Either[String, String] =
    fields.get(name) match
      case Some(PreviewControlJson.Atom.Text(value)) => Right(value)
      case _ => Left(s"metadata field '$name' must be a string")

  private def integer(
      fields: Map[String, PreviewControlJson.Atom],
      name: String,
  ): Either[String, Int] =
    long(fields, name).flatMap: value =>
      Either.cond(
        value >= Int.MinValue && value <= Int.MaxValue,
        value.toInt,
        s"metadata field '$name' is outside the integer range",
      )

  private def unixAddress(value: String): Either[String, Path] =
    try
      val path = Path.of(value)
      Either.cond(
        value.nonEmpty && path.isAbsolute && path.normalize() == path,
        path,
        "metadata field 'address' must be a non-empty absolute normalized path",
      )
    catch
      case error: InvalidPathException =>
        Left(
          s"metadata field 'address' is not a valid path: ${error.getReason}",
        )

  private def long(
      fields: Map[String, PreviewControlJson.Atom],
      name: String,
  ): Either[String, Long] =
    fields.get(name) match
      case Some(PreviewControlJson.Atom.Number(value)) => Right(value)
      case _ => Left(s"metadata field '$name' must be an integer")

  private def check(
      condition: Boolean,
      problem: => String,
  ): Either[String, Unit] =
    Either.cond(condition, (), problem)

private[sbtplugin] enum StaleEndpointDecision:
  case ProtectLive, RemoveStale

private[sbtplugin] object StaleEndpointDecision:
  def decide(connectSucceeded: Boolean): StaleEndpointDecision =
    if connectSucceeded then StaleEndpointDecision.ProtectLive
    else StaleEndpointDecision.RemoveStale

private[sbtplugin] final class PreviewControlServer private (
    server: ServerSocketChannel,
    executor: java.util.concurrent.ExecutorService,
    metadataPath: Path,
    socketPath: Option[Path],
    endpointLease: PreviewEndpointLease,
    metadata: PreviewControlMetadata,
    readyUrl: String,
    onStop: () => Unit,
    log: Logger,
):
  private val closed           = AtomicBoolean(false)
  private val stopRequested    = AtomicBoolean(false)
  private val stopAcknowledged = CountDownLatch(1)
  private val connectionsLock  = Object()
  private val connections      = java.util.HashSet[Connection]()

  def start(): Unit =
    executor.execute(() => acceptLoop())

  def awaitStopAcknowledgedIfRequested(
      timeout: Long,
      unit: TimeUnit,
  ): Boolean =
    !stopRequested.get() || stopAcknowledged.await(timeout, unit)

  def siteChanged(revision: Long): Unit =
    if !closed.get() then
      val message = PreviewControlProtocol.siteChanged(
        sessionId = metadata.sessionId,
        revision = revision,
      )
      connectionSnapshot().foreach(_.siteChanged(message))

  def close(): Unit =
    if closed.compareAndSet(false, true) then
      var failure = Option.empty[Throwable]

      def attempt(action: => Unit): Unit =
        try action
        catch
          case NonFatal(error) =>
            failure match
              case None           => failure = Some(error)
              case Some(existing) => existing.addSuppressed(error)

      attempt(server.close())
      connectionSnapshot().foreach(connection => attempt(connection.close()))
      attempt(executor.shutdownNow(): Unit)
      attempt:
        try
          if !executor.awaitTermination(1L, TimeUnit.SECONDS) then
            log.warn("DocTrellis IPC executor did not stop within one second.")
        catch case _: InterruptedException => Thread.currentThread().interrupt()
      attempt(
        PreviewControlEndpoint.cleanupOwned(
          metadataPath = metadataPath,
          socketPath = socketPath,
          sessionId = metadata.sessionId,
        ),
      )
      attempt(endpointLease.close())
      failure.foreach(throw _)

  private def acceptLoop(): Unit =
    while !closed.get() do
      try
        val client     = server.accept()
        val connection = Connection(client)
        if register(connection) then
          try
            connection.start()
            executor.execute(() => handle(connection))
          catch
            case NonFatal(error) =>
              connection.close()
              throw error
        else connection.close()
      catch
        case NonFatal(error) if !closed.get() =>
          log.warn(
            s"DocTrellis IPC accept failed; retrying: ${error.getMessage}",
          )
          try Thread.sleep(50L)
          catch
            case _: InterruptedException =>
              Thread.currentThread().interrupt()
              if !closed.get() then
                try close()
                catch
                  case NonFatal(closeError) =>
                    log.warn(
                      s"DocTrellis IPC accept loop could not close after interruption: ${closeError.getMessage}",
                    )
        case NonFatal(_) => ()

  private def handle(connection: Connection): Unit =
    val input   = Channels.newInputStream(connection.channel)
    val decoder = PreviewFrameDecoder(PreviewControlProtocol.MaxFrameBytes)
    var authenticatedClient = Option.empty[String]
    var keepReading         = true
    var acknowledgeStop     = false
    val bytes               = Array.ofDim[Byte](2048)

    def send(message: String): Unit =
      if !connection.send(message) then keepReading = false

    try
      while keepReading && !closed.get() do
        val count = input.read(bytes)
        if count < 0 then keepReading = false
        else
          decoder.offer(bytes, count) match
            case Left(problem) =>
              send(PreviewControlProtocol.error("invalidFrame", problem, ""))
              keepReading = false
            case Right(frames) =>
              frames.foreach: frame =>
                if keepReading then
                  PreviewControlProtocol.decodeClient(frame) match
                    case Left(problem) =>
                      send(
                        PreviewControlProtocol.error(
                          "invalidMessage",
                          problem,
                          "",
                        ),
                      )
                      keepReading = false
                    case Right(request) =>
                      request match
                        case PreviewControlRequest.Hello(
                              token,
                              clientId,
                              requestId,
                              siteEvents,
                            ) =>
                          if authenticatedClient.nonEmpty then
                            send(
                              PreviewControlProtocol.error(
                                "duplicateHello",
                                "hello was already accepted",
                                requestId,
                              ),
                            )
                            keepReading = false
                          else if !constantTimeEquals(token, metadata.token)
                          then
                            send(
                              PreviewControlProtocol.error(
                                "authenticationFailed",
                                "workspace IPC authentication failed",
                                requestId,
                              ),
                            )
                            keepReading = false
                          else
                            authenticatedClient = Some(clientId)
                            val accepted = connection.acceptHello(
                              PreviewControlProtocol.ready(
                                url = readyUrl,
                                pid = metadata.pid,
                                sessionId = metadata.sessionId,
                                launchId = metadata.launchId,
                                requestId = requestId,
                              ),
                              siteEvents = siteEvents,
                            )
                            if !accepted then keepReading = false
                        case PreviewControlRequest.Ping(_, _, requestId) =>
                          if authenticate(
                              request,
                              authenticatedClient,
                              requestId,
                              send,
                            )
                          then send(PreviewControlProtocol.pong(requestId))
                          else keepReading = false
                        case PreviewControlRequest.Stop(_, _, requestId) =>
                          if authenticate(
                              request,
                              authenticatedClient,
                              requestId,
                              send,
                            )
                          then
                            stopRequested.set(true)
                            send(PreviewControlProtocol.stopping(requestId))
                            try
                              onStop()
                              send(PreviewControlProtocol.stopped(requestId))
                            catch
                              case NonFatal(error) =>
                                send(
                                  PreviewControlProtocol.error(
                                    "stopFailed",
                                    Option(error.getMessage)
                                      .filter(_.nonEmpty)
                                      .getOrElse(error.toString),
                                    requestId,
                                  ),
                                )
                            acknowledgeStop = true
                            keepReading = false
    catch
      case NonFatal(error) if !closed.get() =>
        log.warn(s"DocTrellis IPC client ended: ${error.getMessage}")
      case NonFatal(_) => ()
    finally
      val afterWrites =
        if acknowledgeStop then () => stopAcknowledged.countDown()
        else () => ()
      if !connection.finish(afterWrites) && acknowledgeStop then
        stopAcknowledged.countDown()

  private def register(connection: Connection): Boolean =
    connectionsLock.synchronized:
      if closed.get() then false
      else
        connections.add(connection)
        true

  private def unregister(connection: Connection): Unit =
    connectionsLock.synchronized(connections.remove(connection): Unit)

  private def connectionSnapshot(): Vector[Connection] =
    connectionsLock.synchronized:
      val result   = Vector.newBuilder[Connection]
      val iterator = connections.iterator()
      while iterator.hasNext do result += iterator.next()
      result.result()

  private final class Connection(val channel: SocketChannel):
    private val terminated = AtomicBoolean(false)
    private val writer     = PreviewFrameWriter(
      output = Channels.newOutputStream(channel),
      executor = executor,
      onFailure = error =>
        if !closed.get() then
          log.warn(s"DocTrellis IPC writer ended: ${error.getMessage}")
        terminate()
      ,
      onTerminated = () => terminate(),
    )
    private val events = PreviewConnectionEvents(writer.send)

    def start(): Unit = writer.start()

    def send(message: String): Boolean = writer.send(message)

    def acceptHello(message: String, siteEvents: Boolean): Boolean =
      events.acceptHello(message, siteEvents)

    def siteChanged(message: String): Unit = events.siteChanged(message)

    def finish(afterWrites: () => Unit): Boolean = writer.finish(afterWrites)

    def close(): Unit =
      events.close()
      writer.abort()
      terminate()

    private def terminate(): Unit =
      if terminated.compareAndSet(false, true) then
        events.close()
        unregister(this)
        try channel.close()
        catch
          case NonFatal(error) if !closed.get() =>
            log.warn(
              s"DocTrellis IPC connection could not close: ${error.getMessage}",
            )
          case NonFatal(_) => ()

  private def authenticate(
      request: PreviewControlRequest,
      authenticatedClient: Option[String],
      requestId: String,
      send: String => Unit,
  ): Boolean =
    val valid = authenticatedClient.contains(request.clientId) &&
      constantTimeEquals(request.token, metadata.token)
    if !valid then
      send(
        PreviewControlProtocol.error(
          "authenticationFailed",
          "workspace IPC authentication failed",
          requestId,
        ),
      )
    valid

  private def constantTimeEquals(left: String, right: String): Boolean =
    MessageDigest.isEqual(
      left.getBytes(StandardCharsets.UTF_8),
      right.getBytes(StandardCharsets.UTF_8),
    )

private[sbtplugin] object PreviewControlServer:
  def open(
      config: PreviewControlConfig,
      readyUrl: String,
      onStop: () => Unit,
      log: Logger,
  ): PreviewControlServer =
    val sessionId = UUID.randomUUID().toString
    val token     = randomToken()
    val pid       = ProcessHandle.current().pid()
    val paths    = PreviewControlEndpoint.prepare(config, pid, sessionId, token)
    var server   = Option.empty[ServerSocketChannel]
    var executor = Option.empty[java.util.concurrent.ExecutorService]
    try
      val bound = paths.transport match
        case PreviewControlTransport.Unix(address) =>
          val channel = ServerSocketChannel.open(StandardProtocolFamily.UNIX)
          channel.bind(UnixDomainSocketAddress.of(address))
          PreviewControlEndpoint.ownerOnly(address, file = true)
          channel
        case PreviewControlTransport.Tcp(host, _) =>
          val channel = ServerSocketChannel.open()
          channel.bind(InetSocketAddress(host, 0))
          channel
      server = Some(bound)
      val transport = paths.transport match
        case unix @ PreviewControlTransport.Unix(_) => unix
        case PreviewControlTransport.Tcp(host, _)   =>
          bound.getLocalAddress match
            case address: InetSocketAddress =>
              PreviewControlTransport.Tcp(host, address.getPort)
            case address =>
              throw IllegalStateException(
                s"DocTrellis TCP control channel received an unexpected local address: $address",
              )
      val metadata = PreviewControlMetadata(
        transport = transport,
        pid = pid,
        sessionId = sessionId,
        launchId = config.launchId,
        token = token,
      )
      PreviewControlEndpoint.publish(paths.metadataPath, metadata)
      val connectionExecutor = Executors.newCachedThreadPool(runnable =>
        val thread = Thread(runnable, "doctrellis-preview-ipc")
        thread.setDaemon(true)
        thread,
      )
      executor = Some(connectionExecutor)
      val control = PreviewControlServer(
        server = bound,
        executor = connectionExecutor,
        metadataPath = paths.metadataPath,
        socketPath = paths.socketPath,
        endpointLease = paths.lease,
        metadata = metadata,
        readyUrl = readyUrl,
        onStop = onStop,
        log = log,
      )
      control.start()
      control
    catch
      case error: Throwable =>
        def cleanup(action: => Unit): Unit =
          try action
          catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)

        cleanup(server.foreach(_.close()))
        cleanup(executor.foreach(_.shutdownNow(): Unit))
        cleanup(
          PreviewControlEndpoint.cleanupOwned(
            metadataPath = paths.metadataPath,
            socketPath = paths.socketPath,
            sessionId = sessionId,
          ),
        )
        cleanup(paths.lease.close())
        throw error

  private def randomToken(): String =
    val bytes = Array.ofDim[Byte](32)
    SecureRandom().nextBytes(bytes)
    bytes.iterator.map(byte => f"${byte & 0xff}%02x").mkString

private[sbtplugin] final case class PreviewControlPaths(
    metadataPath: Path,
    socketPath: Option[Path],
    transport: PreviewControlTransport,
    lease: PreviewEndpointLease,
)

private[sbtplugin] final class PreviewEndpointLease private (
    channel: FileChannel,
    lock: FileLock,
):
  private val closed = AtomicBoolean(false)

  def close(): Unit =
    if closed.compareAndSet(false, true) then
      try lock.release()
      finally channel.close()

private[sbtplugin] object PreviewEndpointLease:
  def acquire(path: Path): PreviewEndpointLease =
    val channel = FileChannel.open(
      path,
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE,
    )
    try
      PreviewControlEndpoint.ownerOnly(path, file = true)
      val lock = try Option(channel.tryLock())
      catch case _: OverlappingFileLockException => None
      lock
        .map(value => PreviewEndpointLease(channel, value))
        .getOrElse(
          throw IllegalStateException(
            "A live DocTrellis preview already owns this workspace IPC endpoint.",
          ),
        )
    catch
      case error: Throwable =>
        channel.close()
        throw error

private[sbtplugin] object PreviewControlEndpoint:
  private val UnixPathLimit = 100

  def prepare(
      config: PreviewControlConfig,
      pid: Long,
      sessionId: String,
      token: String,
  ): PreviewControlPaths =
    prepare(config, pid, sessionId, token, isWindows)

  def prepare(
      config: PreviewControlConfig,
      pid: Long,
      sessionId: String,
      token: String,
      windows: Boolean,
  ): PreviewControlPaths =
    Files.createDirectories(config.runtimeDirectory)
    verifyRuntimeDirectory(config)
    ownerOnly(config.runtimeDirectory, file = false)
    val metadataPath =
      config.runtimeDirectory.resolve(s"${config.workspaceHash}.json")
    val lockPath =
      config.runtimeDirectory.resolve(s"${config.workspaceHash}.lock")
    val socketPath = Option.unless(windows)(
      config.runtimeDirectory.resolve(
        s"${config.workspaceHash}.${PreviewControlConfig.shortHash(sessionId).take(16)}.sock",
      ),
    )
    socketPath.foreach: path =>
      val length = path.toString.getBytes(StandardCharsets.UTF_8).length
      if length > UnixPathLimit then
        throw IllegalArgumentException(
          s"DocTrellis IPC socket path is $length bytes; maximum is $UnixPathLimit.",
        )
    val provisional = PreviewControlMetadata(
      transport = socketPath
        .map(PreviewControlTransport.Unix(_))
        .getOrElse(PreviewControlTransport.Tcp("127.0.0.1", 0)),
      pid = pid,
      sessionId = sessionId,
      launchId = config.launchId,
      token = token,
    )
    val lease = PreviewEndpointLease.acquire(lockPath)
    try
      if Files.exists(metadataPath, LinkOption.NOFOLLOW_LINKS) then
        val existing = readRecoverable(metadataPath)
        val decision = existing match
          case None           => StaleEndpointDecision.RemoveStale
          case Some(metadata) =>
            StaleEndpointDecision.decide(
              connectSucceeded = canConnect(metadata),
            )
        decision match
          case StaleEndpointDecision.ProtectLive =>
            throw IllegalStateException(
              "A live DocTrellis preview already owns this workspace IPC endpoint.",
            )
          case StaleEndpointDecision.RemoveStale =>
            existing
              .flatMap(staleSocket(config, _))
              .foreach(Files.deleteIfExists)
            Files.deleteIfExists(metadataPath): Unit
      publishProvisional(metadataPath, provisional)
      PreviewControlPaths(
        metadataPath = metadataPath,
        socketPath = socketPath,
        transport = provisional.transport,
        lease = lease,
      )
    catch
      case error: Throwable =>
        try lease.close()
        catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)
        throw error

  def publish(path: Path, metadata: PreviewControlMetadata): Unit =
    val temporary =
      path.resolveSibling(s".${path.getFileName}.${metadata.sessionId}.tmp")
    try
      Files.writeString(
        temporary,
        metadata.json,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE,
      )
      ownerOnly(temporary, file = true)
      Files.move(
        temporary,
        path,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING,
      ): Unit
    finally Files.deleteIfExists(temporary): Unit

  def cleanupOwned(
      metadataPath: Path,
      socketPath: Option[Path],
      sessionId: String,
  ): Unit =
    val owned = readRecoverable(metadataPath).exists(_.sessionId == sessionId)
    if owned then
      socketPath.foreach(Files.deleteIfExists)
      Files.deleteIfExists(metadataPath): Unit

  def ownerOnly(path: Path, file: Boolean): Unit =
    val permissions =
      if file then PosixFilePermissions.fromString("rw-------")
      else PosixFilePermissions.fromString("rwx------")
    try Files.setPosixFilePermissions(path, permissions): Unit
    catch case _: UnsupportedOperationException => ()

  private def verifyRuntimeDirectory(config: PreviewControlConfig): Unit =
    val attributes = Files.readAttributes(
      config.runtimeDirectory,
      classOf[BasicFileAttributes],
      LinkOption.NOFOLLOW_LINKS,
    )
    if !attributes.isDirectory || attributes.isSymbolicLink then
      throw IllegalStateException(
        "The DocTrellis IPC runtime path is not a real directory.",
      )
    config.runtimeOwner.foreach: expected =>
      val attribute = Files.getAttribute(
        config.runtimeDirectory,
        "unix:uid",
        LinkOption.NOFOLLOW_LINKS,
      )
      val actual = attribute match
        case number: Number => number.longValue()
        case other          =>
          throw IllegalStateException(
            s"DocTrellis IPC runtime path returned a non-numeric unix:uid attribute: ${String.valueOf(other)}",
          )
      if actual != expected then
        throw IllegalStateException(
          "The DocTrellis IPC runtime directory is not owned by the current user.",
        )

  private def staleSocket(
      config: PreviewControlConfig,
      metadata: PreviewControlMetadata,
  ): Option[Path] =
    metadata.transport match
      case PreviewControlTransport.Unix(address) =>
        val normalized     = address.toAbsolutePath.normalize()
        val name           = normalized.getFileName.toString
        val expectedPrefix = s"${config.workspaceHash}."
        Option.when(
          normalized.getParent == config.runtimeDirectory &&
            name.startsWith(expectedPrefix) && name.endsWith(".sock"),
        )(normalized)
      case PreviewControlTransport.Tcp(_, _) => None

  private def readRecoverable(path: Path): Option[PreviewControlMetadata] =
    try
      val value = Files.readString(path)
      PreviewControlMetadata.parse(value) match
        case Right(metadata) => Some(metadata)
        case Left(_) => PreviewControlMetadata.parseProvisional(value).toOption
    catch case NonFatal(_) => None

  private def publishProvisional(
      path: Path,
      metadata: PreviewControlMetadata,
  ): Unit =
    val temporary =
      path.resolveSibling(s".${path.getFileName}.${metadata.sessionId}.tmp")
    val value = metadata.transport match
      case PreviewControlTransport.Tcp(_, 0) =>
        PreviewControlMetadata.provisionalJson(metadata)
      case _ => metadata.json
    try
      Files.writeString(
        temporary,
        value,
        StandardCharsets.UTF_8,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE,
      )
      ownerOnly(temporary, file = true)
      Files.move(
        temporary,
        path,
        StandardCopyOption.ATOMIC_MOVE,
      ): Unit
    finally Files.deleteIfExists(temporary): Unit

  private def isWindows: Boolean =
    System
      .getProperty("os.name", "")
      .toLowerCase(java.util.Locale.ROOT)
      .contains("win")

  private def canConnect(metadata: PreviewControlMetadata): Boolean =
    metadata.transport match
      case PreviewControlTransport.Tcp(_, port) if port <= 0 => false
      case connectable                                       =>
        var channel = Option.empty[SocketChannel]
        try
          val (opened, address) = connectable match
            case PreviewControlTransport.Unix(path) =>
              SocketChannel.open(StandardProtocolFamily.UNIX) ->
                UnixDomainSocketAddress.of(path)
            case PreviewControlTransport.Tcp(host, port) =>
              SocketChannel.open() -> InetSocketAddress(host, port)
          channel = Some(opened)
          opened.configureBlocking(false)
          var connected = opened.connect(address)
          val deadline  = System.nanoTime() + 500000000L
          while !connected && System.nanoTime() < deadline do
            connected = opened.finishConnect()
            if !connected then Thread.sleep(5L)
          if !connected then false
          else authenticateProbe(opened, metadata, deadline)
        catch case NonFatal(_) => false
        finally
          channel.foreach(value =>
            try value.close()
            catch case NonFatal(_) => (),
          )

  private def authenticateProbe(
      channel: SocketChannel,
      metadata: PreviewControlMetadata,
      deadline: Long,
  ): Boolean =
    val clientId  = UUID.randomUUID().toString
    val requestId = UUID.randomUUID().toString
    val request   = PreviewControlJson.objectValue(
      Vector(
        "version"   -> "1",
        "type"      -> PreviewControlJson.quoted("hello"),
        "token"     -> PreviewControlJson.quoted(metadata.token),
        "clientId"  -> PreviewControlJson.quoted(clientId),
        "requestId" -> PreviewControlJson.quoted(requestId),
      ),
    ) + "\n"
    val output = ByteBuffer.wrap(request.getBytes(StandardCharsets.UTF_8))
    while output.hasRemaining && System.nanoTime() < deadline do
      channel.write(output)
      if output.hasRemaining then Thread.sleep(5L)
    if output.hasRemaining then false
    else
      val decoder = PreviewFrameDecoder(PreviewControlProtocol.MaxFrameBytes)
      val input   = ByteBuffer.allocate(2048)
      var ready   = false
      var open    = true
      while !ready && open && System.nanoTime() < deadline do
        input.clear()
        val count = channel.read(input)
        if count < 0 then open = false
        else if count == 0 then Thread.sleep(5L)
        else
          decoder.offer(input.array(), count) match
            case Left(_)         => open = false
            case Right(complete) =>
              ready = complete.exists(frame =>
                PreviewControlProtocol.isReadyResponse(
                  frame,
                  requestId,
                  metadata,
                ),
              )
      ready

private[sbtplugin] final class PreviewFrameDecoder(maxFrameBytes: Int):
  private val pending = ByteArrayOutputStream()

  def offer(bytes: Array[Byte], count: Int): Either[String, Vector[String]] =
    if count < 0 || count > bytes.length then
      Left(s"IPC byte count $count is outside the input array bounds")
    else
      val frames = Vector.newBuilder[String]
      var index  = 0
      var error  = Option.empty[String]
      while index < count && error.isEmpty do
        val byte = bytes(index)
        if byte == '\n'.toByte then
          val raw = pending.toByteArray
          pending.reset()
          val length = if raw.lastOption.contains('\r'.toByte) then
            raw.length - 1
          else raw.length
          if length == 0 then error = Some("IPC frame must not be empty")
          else
            decodeUtf8(raw, length) match
              case Right(frame)  => frames += frame
              case Left(problem) => error = Some(problem)
        else
          pending.write(byte.toInt)
          val pendingBytes                  = pending.size()
          val allowedTrailingCarriageReturn =
            pendingBytes == maxFrameBytes + 1 && byte == '\r'.toByte
          if pendingBytes > maxFrameBytes && !allowedTrailingCarriageReturn then
            error = Some(s"IPC frame exceeds $maxFrameBytes bytes")
        index += 1
      error.toLeft(frames.result())

  private def decodeUtf8(
      bytes: Array[Byte],
      length: Int,
  ): Either[String, String] =
    try
      val decoder = StandardCharsets.UTF_8
        .newDecoder()
        .onMalformedInput(CodingErrorAction.REPORT)
        .onUnmappableCharacter(CodingErrorAction.REPORT)
      Right(decoder.decode(ByteBuffer.wrap(bytes, 0, length)).toString)
    catch
      case _: CharacterCodingException =>
        Left("IPC frame must contain valid UTF-8")

private[sbtplugin] final class PreviewFrameWriter(
    output: java.io.OutputStream,
    executor: Executor,
    onFailure: Throwable => Unit,
    onTerminated: () => Unit,
):
  import PreviewFrameWriter.Command

  private val stateLock   = Object()
  private val queue       = LinkedBlockingQueue[Command]()
  private val aborted     = AtomicBoolean(false)
  private val started     = AtomicBoolean(false)
  private val completed   = AtomicBoolean(false)
  private val terminated  = CountDownLatch(1)
  private var accepting   = true
  private var afterWrites = Option.empty[() => Unit]

  def start(): Unit =
    if !started.compareAndSet(false, true) then
      throw IllegalStateException("DocTrellis IPC writer was already started.")
    try executor.execute(() => writeLoop())
    catch
      case error: Throwable =>
        stateLock.synchronized:
          accepting = false
        complete()
        throw error

  def send(message: String): Boolean =
    stateLock.synchronized:
      if accepting then
        queue.add(Command.Frame(message))
        true
      else false

  def finish(afterWrites: () => Unit = () => ()): Boolean =
    stateLock.synchronized:
      if accepting then
        accepting = false
        this.afterWrites = Some(afterWrites)
        queue.add(Command.Finish)
        true
      else false

  def abort(): Unit =
    stateLock.synchronized:
      aborted.set(true)
      accepting = false
      queue.clear()
      queue.offer(Command.Finish)
      (): Unit
    if !started.get() then complete()

  def awaitTermination(timeout: Long, unit: TimeUnit): Boolean =
    terminated.await(timeout, unit)

  private def writeLoop(): Unit =
    var continue = true
    try
      while continue do
        queue.take() match
          case Command.Frame(message) =>
            output.write(message.getBytes(StandardCharsets.UTF_8))
            output.flush()
          case Command.Finish => continue = false
    catch
      case _: InterruptedException => Thread.currentThread().interrupt()
      case NonFatal(error)         =>
        stopAccepting()
        notifyFailure(error)
    finally
      stopAccepting()
      complete()

  private def stopAccepting(): Unit =
    stateLock.synchronized:
      accepting = false
      queue.clear()

  private def complete(): Unit =
    if completed.compareAndSet(false, true) then
      val finishCallback = stateLock.synchronized:
        val callback = afterWrites
        afterWrites = None
        callback
      try
        try finishCallback.foreach(callback => callback())
        catch case NonFatal(error) => notifyFailure(error)
      finally
        try onTerminated()
        finally terminated.countDown()

  private def notifyFailure(error: Throwable): Unit =
    if !aborted.get() then
      try onFailure(error)
      catch case NonFatal(callbackError) => error.addSuppressed(callbackError)

private object PreviewFrameWriter:
  private enum Command:
    case Frame(message: String)
    case Finish

private[sbtplugin] final class PreviewConnectionEvents(
    send: String => Boolean,
):
  private val lock    = Object()
  private var enabled = false

  def acceptHello(message: String, siteEvents: Boolean): Boolean =
    lock.synchronized:
      val accepted = send(message)
      if accepted then enabled = siteEvents
      accepted

  def siteChanged(message: String): Unit =
    lock.synchronized:
      if enabled then send(message): Unit

  def close(): Unit =
    lock.synchronized:
      enabled = false

private[sbtplugin] enum PreviewControlRequest:
  def token: String
  def clientId: String
  case Hello(
      token: String,
      clientId: String,
      requestId: String,
      siteEvents: Boolean,
  )
  case Ping(token: String, clientId: String, requestId: String)
  case Stop(token: String, clientId: String, requestId: String)

private[sbtplugin] object PreviewControlProtocol:
  val MaxFrameBytes = 8192

  def decodeClient(frame: String): Either[String, PreviewControlRequest] =
    PreviewControlJson
      .parse(frame)
      .flatMap: fields =>
        for
          version <- number(fields, "version")
          _    <- check(version == 1L, s"unknown IPC protocol version $version")
          kind <- text(fields, "type")
          token   <- text(fields, "token")
          client  <- nonEmpty(fields, "clientId")
          request <- nonEmpty(fields, "requestId")
          message <- kind match
            case "hello" =>
              Right(
                PreviewControlRequest.Hello(
                  token,
                  client,
                  request,
                  siteEvents(fields),
                ),
              )
            case "ping" =>
              Right(PreviewControlRequest.Ping(token, client, request))
            case "stop" =>
              Right(PreviewControlRequest.Stop(token, client, request))
            case other => Left(s"unknown IPC message type '$other'")
        yield message

  def ready(
      url: String,
      pid: Long,
      sessionId: String,
      launchId: String,
      requestId: String,
  ): String =
    response(
      kind = "ready",
      requestId = requestId,
      extra = Vector(
        "url"       -> PreviewControlJson.quoted(url),
        "pid"       -> pid.toString,
        "sessionId" -> PreviewControlJson.quoted(sessionId),
        "launchId"  -> PreviewControlJson.quoted(launchId),
      ),
    )

  def pong(requestId: String): String =
    response(kind = "pong", requestId = requestId, extra = Vector.empty)

  def stopping(requestId: String): String =
    response(kind = "stopping", requestId = requestId, extra = Vector.empty)

  def stopped(requestId: String): String =
    response(kind = "stopped", requestId = requestId, extra = Vector.empty)

  def siteChanged(sessionId: String, revision: Long): String =
    PreviewControlJson.objectValue(
      Vector(
        "version"   -> "1",
        "type"      -> PreviewControlJson.quoted("siteChanged"),
        "sessionId" -> PreviewControlJson.quoted(sessionId),
        "revision"  -> revision.toString,
      ),
    ) + "\n"

  def error(code: String, message: String, requestId: String): String =
    val encodedCode =
      Option(code).filter(_.nonEmpty).getOrElse("internal")
    val encodedMessage =
      Option(message).filter(_.nonEmpty).getOrElse("unspecified IPC error")
    response(
      kind = "error",
      requestId = Option(requestId).getOrElse(""),
      extra = Vector(
        "code"    -> PreviewControlJson.quoted(encodedCode),
        "message" -> PreviewControlJson.quoted(encodedMessage),
      ),
    )

  def isReadyResponse(
      frame: String,
      requestId: String,
      metadata: PreviewControlMetadata,
  ): Boolean =
    PreviewControlJson
      .parse(frame)
      .exists: fields =>
        fields.get("version").contains(PreviewControlJson.Atom.Number(1L)) &&
          fields
            .get("type")
            .contains(PreviewControlJson.Atom.Text("ready")) &&
          fields
            .get("requestId")
            .contains(PreviewControlJson.Atom.Text(requestId)) &&
          fields
            .get("sessionId")
            .contains(PreviewControlJson.Atom.Text(metadata.sessionId)) &&
          fields
            .get("launchId")
            .contains(PreviewControlJson.Atom.Text(metadata.launchId)) &&
          fields
            .get("pid")
            .contains(PreviewControlJson.Atom.Number(metadata.pid)) &&
          fields
            .get("url")
            .exists:
              case PreviewControlJson.Atom.Text(value) =>
                value.startsWith("http://127.0.0.1:")
              case PreviewControlJson.Atom.Number(_) => false

  private def response(
      kind: String,
      requestId: String,
      extra: Vector[(String, String)],
  ): String =
    PreviewControlJson.objectValue(
      Vector(
        "version"   -> "1",
        "type"      -> PreviewControlJson.quoted(kind),
        "requestId" -> PreviewControlJson.quoted(requestId),
      ) ++ extra,
    ) + "\n"

  private def text(
      fields: Map[String, PreviewControlJson.Atom],
      name: String,
  ): Either[String, String] =
    fields.get(name) match
      case Some(PreviewControlJson.Atom.Text(value)) => Right(value)
      case _ => Left(s"IPC field '$name' must be a string")

  private def nonEmpty(
      fields: Map[String, PreviewControlJson.Atom],
      name: String,
  ): Either[String, String] =
    text(fields, name).flatMap(value =>
      Either.cond(value.nonEmpty, value, s"IPC field '$name' must not be empty"),
    )

  private def number(
      fields: Map[String, PreviewControlJson.Atom],
      name: String,
  ): Either[String, Long] =
    fields.get(name) match
      case Some(PreviewControlJson.Atom.Number(value)) => Right(value)
      case _ => Left(s"IPC field '$name' must be an integer")

  private def siteEvents(
      fields: Map[String, PreviewControlJson.Atom],
  ): Boolean =
    fields
      .get("siteEvents")
      .contains(PreviewControlJson.Atom.Number(1L))

  private def check(
      condition: Boolean,
      problem: => String,
  ): Either[String, Unit] =
    Either.cond(condition, (), problem)

private[sbtplugin] object PreviewControlJson:
  enum Atom:
    case Text(value: String)
    case Number(value: Long)

  def parse(value: String): Either[String, Map[String, Atom]] =
    JsonCursor(value).parseObject

  def objectValue(fields: Vector[(String, String)]): String =
    fields
      .map((name, value) => s"${quoted(name)}:$value")
      .mkString("{", ",", "}")

  def quoted(value: String): String =
    val result = StringBuilder("\"")
    value.foreach:
      case '"'                => result.append("\\\"")
      case '\\'               => result.append("\\\\")
      case '\b'               => result.append("\\b")
      case '\f'               => result.append("\\f")
      case '\n'               => result.append("\\n")
      case '\r'               => result.append("\\r")
      case '\t'               => result.append("\\t")
      case char if char < ' ' => result.append(f"\\u${char.toInt}%04x")
      case char               => result.append(char)
    result.append('"').toString

  private final class JsonCursor private (value: String):
    private var index = 0

    def parseObject: Either[String, Map[String, Atom]] =
      for
        _      <- consume('{')
        fields <- parseFields(Map.empty)
        _      <- consume('}')
        _      <- trailing
      yield fields

    @tailrec
    private def whitespace(): Unit =
      if index < value.length && value.charAt(index).isWhitespace then
        index += 1
        whitespace()

    private def parseFields(
        accumulated: Map[String, Atom],
    ): Either[String, Map[String, Atom]] =
      whitespace()
      if peek('}') then Right(accumulated)
      else
        for
          name <- string()
          _    <- Either.cond(
            !accumulated.contains(name),
            (),
            s"duplicate JSON field '$name'",
          )
          _    <- consume(':')
          atom <- valueAtom()
          next = accumulated.updated(name, atom)
          result <-
            whitespace()
            if peek(',') then
              index += 1
              whitespace()
              if peek('}') then Left("trailing JSON comma")
              else parseFields(next)
            else Right(next)
        yield result

    private def valueAtom(): Either[String, Atom] =
      whitespace()
      if peek('"') then string().map(Atom.Text(_))
      else number().map(Atom.Number(_))

    private def string(): Either[String, String] =
      consume('"').flatMap: _ =>
        val result = StringBuilder()
        var done   = false
        var error  = Option.empty[String]
        while !done && error.isEmpty && index < value.length do
          val char = value.charAt(index)
          index += 1
          char match
            case '"'  => done = true
            case '\\' =>
              if index >= value.length then
                error = Some("unterminated JSON escape")
              else
                val escaped = value.charAt(index)
                index += 1
                escaped match
                  case '"'  => result.append('"')
                  case '\\' => result.append('\\')
                  case '/'  => result.append('/')
                  case 'b'  => result.append('\b')
                  case 'f'  => result.append('\f')
                  case 'n'  => result.append('\n')
                  case 'r'  => result.append('\r')
                  case 't'  => result.append('\t')
                  case 'u'  =>
                    if index + 4 > value.length then
                      error = Some("incomplete JSON unicode escape")
                    else
                      val digits = value.substring(index, index + 4)
                      if digits.forall(isHex) then
                        result.append(Integer.parseInt(digits, 16).toChar)
                        index += 4
                      else error = Some("invalid JSON unicode escape")
                  case _ => error = Some("invalid JSON escape")
            case control if control < ' ' =>
              error = Some("unescaped JSON control character")
            case other => result.append(other)
        error match
          case Some(problem) => Left(problem)
          case None if done  => Right(result.toString)
          case None          => Left("unterminated JSON string")

    private def number(): Either[String, Long] =
      val start = index
      if peek('-') then index += 1
      if index >= value.length || !value.charAt(index).isDigit then
        Left("expected a JSON integer")
      else if value.charAt(index) == '0' then
        index += 1
        if index < value.length && value.charAt(index).isDigit then
          Left("JSON integers must not contain leading zeros")
        else
          value.substring(start, index).toLongOption.toRight("invalid integer")
      else
        while index < value.length && value.charAt(index).isDigit do index += 1
        value.substring(start, index).toLongOption.toRight("invalid integer")

    private def consume(expected: Char): Either[String, Unit] =
      whitespace()
      Either.cond(
        index < value.length && value.charAt(index) == expected,
        { index += 1; () },
        s"expected '$expected' at offset $index",
      )

    private def trailing: Either[String, Unit] =
      whitespace()
      Either.cond(
        index == value.length,
        (),
        s"unexpected JSON at offset $index",
      )

    private def peek(expected: Char): Boolean =
      index < value.length && value.charAt(index) == expected

    private def isHex(char: Char): Boolean =
      char.isDigit || (char >= 'a' && char <= 'f') || (char >= 'A' && char <= 'F')

  private object JsonCursor:
    def apply(value: String): JsonCursor = new JsonCursor(value)
