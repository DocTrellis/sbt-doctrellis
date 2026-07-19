package org.doctrellis.vscode.infrastructure

import cats.effect.std.Dispatcher
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import org.doctrellis.vscode.application.RunningProcess
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

import scala.concurrent.duration.*
import scala.scalajs.js

private[infrastructure] final case class WorkspaceChannelEndpoint(
    canonicalWorkspace: String,
    runtimeDirectory: String,
    metadataPath: String,
    socketPrefix: String,
    workspaceHash: String,
    runtimeOwner: Option[Int],
):
  def environment(launchId: String): Map[String, String] =
    Map(
      WorkspaceChannel.ProtocolEnvironment  -> WorkspaceChannel.Protocol,
      WorkspaceChannel.RuntimeEnvironment   -> runtimeDirectory,
      WorkspaceChannel.HashEnvironment      -> workspaceHash,
      WorkspaceChannel.WorkspaceEnvironment -> canonicalWorkspace,
      WorkspaceChannel.LaunchEnvironment    -> launchId,
    ) ++ runtimeOwner.map(value =>
      WorkspaceChannel.OwnerEnvironment -> value.toString,
    )

private[infrastructure] enum WorkspaceChannelTransport:
  case Unix(path: String)
  case Tcp(host: String, port: Int)

private[infrastructure] final case class WorkspaceChannelMetadata(
    transport: WorkspaceChannelTransport,
    pid: Int,
    sessionId: String,
    launchId: String,
    token: String,
)

private[infrastructure] final case class WorkspaceLaunchMismatch(
    expected: String,
    actual: String,
) extends IllegalStateException(
      "the workspace IPC endpoint belongs to a different preview launch",
    )

private[infrastructure] final case class WorkspaceMetadataFailure(
    detail: String,
) extends IllegalStateException(detail):
  def retryableDuringPublication: Boolean = detail == "IPC TCP port is invalid"

private[infrastructure] object WorkspaceChannel:
  val Protocol             = "doctrellis-preview-v1"
  val ProtocolEnvironment  = "DOCTRELLIS_PREVIEW_PROTOCOL"
  val RuntimeEnvironment   = "DOCTRELLIS_RUNTIME_DIR"
  val HashEnvironment      = "DOCTRELLIS_WORKSPACE_HASH"
  val WorkspaceEnvironment = "DOCTRELLIS_WORKSPACE_CANONICAL"
  val OwnerEnvironment     = "DOCTRELLIS_RUNTIME_OWNER"
  val LaunchEnvironment    = "DOCTRELLIS_LAUNCH_ID"
  val MaxFrameBytes        = 8192

  def endpoint(workspace: Workspace): IO[WorkspaceChannelEndpoint] =
    for
      canonical <- promise(
        NodeFsPromises.realpath(workspace.root),
        "could not canonicalize the workspace for its IPC endpoint",
      )
      user          = NodeOs.userInfo()
      platform      = ProcessPlatform.parse(NodeRuntime.platform)
      userHash      = digest(user.username).take(12)
      workspaceHash = digest(canonical).take(24)
      runtime       =
        if platform == ProcessPlatform.Windows
        then NodePath.join(NodeOs.tmpdir(), s"doctrellis-$userHash")
        else NodePath.join("/tmp", s"doctrellis-$userHash")
      _ <- promise(
        NodeFsPromises.mkdir(
          runtime,
          js.Dynamic.literal(recursive = true, mode = 448),
        ),
        "could not create the DocTrellis IPC runtime directory",
      )
      owner = Option.when(platform == ProcessPlatform.Posix)(user.uid)
      _ <- owner.traverse_(verifyRuntimeDirectory(runtime, _))
    yield WorkspaceChannelEndpoint(
      canonicalWorkspace = canonical,
      runtimeDirectory = runtime,
      metadataPath = NodePath.join(runtime, s"$workspaceHash.json"),
      socketPrefix = NodePath.join(runtime, s"$workspaceHash."),
      workspaceHash = workspaceHash,
      runtimeOwner = owner,
    )

  private[infrastructure] def verifyRuntimeDirectory(
      path: String,
      expectedOwner: Int,
  ): IO[Unit] =
    for
      attributes <- promise(
        NodeFsPromises.lstat(path),
        "could not inspect the DocTrellis IPC runtime directory",
      )
      _ <- IO.raiseWhen(
        !attributes.isDirectory() || attributes.isSymbolicLink(),
      )(
        IllegalStateException(
          "the DocTrellis IPC runtime path is not a real directory",
        ),
      )
      _ <- IO.raiseWhen(attributes.uid != expectedOwner)(
        IllegalStateException(
          "the DocTrellis IPC runtime directory is not owned by the current user",
        ),
      )
      _ <- promise(
        NodeFsPromises.chmod(path, 448),
        "could not restrict the DocTrellis IPC runtime directory",
      )
    yield ()

  def connect(
      dispatcher: Dispatcher[IO],
      endpoint: WorkspaceChannelEndpoint,
      expectedLaunchId: Option[String],
  ): IO[WorkspaceChannelConnection] =
    for
      text <- promise(
        NodeFsPromises.readFile(endpoint.metadataPath, "utf8"),
        "the workspace IPC endpoint is not available",
      )
      metadata <- IO.fromEither(
        WorkspaceChannelMetadataParser
          .parse(text, endpoint)
          .leftMap(WorkspaceMetadataFailure(_)),
      )
      _ <- expectedLaunchId
        .filter(_ != metadata.launchId)
        .traverse_(expected =>
          IO.raiseError(
            WorkspaceLaunchMismatch(expected, metadata.launchId),
          ),
        )
      channel <- IO.uncancelable: poll =>
        poll(connectSocket(metadata.transport)).flatMap: socket =>
          poll(
            WorkspaceChannelConnection.create(
              dispatcher = dispatcher,
              socket = socket,
              metadata = metadata,
            ),
          ).attempt
            .onCancel(IO.delay(socket.destroy()))
            .flatMap:
              case Right(connection) => IO.pure(connection)
              case Left(error)       =>
                IO.delay(socket.destroy()) *> IO.raiseError(error)
    yield channel

  def connectReady(
      dispatcher: Dispatcher[IO],
      endpoint: WorkspaceChannelEndpoint,
      verifyPing: Boolean,
      expectedLaunchId: Option[String],
  ): IO[WorkspaceChannelConnection] =
    IO.uncancelable: poll =>
      poll(connect(dispatcher, endpoint, expectedLaunchId)).flatMap:
        connection =>
          val validate = connection.awaitUrl *>
            IO.whenA(verifyPing)(connection.ping)
          poll(validate.as(connection)).attempt
            .onCancel(connection.close)
            .flatMap:
              case Right(verified) => IO.pure(verified)
              case Left(error)     => connection.close *> IO.raiseError(error)

  def endpointOwnerAlive(endpoint: WorkspaceChannelEndpoint): IO[Boolean] =
    promise(
      NodeFsPromises.readFile(endpoint.metadataPath, "utf8"),
      "could not read the workspace IPC endpoint owner",
    )
      .flatMap: text =>
        IO.fromEither(
          WorkspaceChannelMetadataParser
            .ownerPid(text)
            .leftMap(WorkspaceMetadataFailure(_)),
        )
      .flatMap: pid =>
        IO.delay(NodeRuntime.kill(pid, 0))
          .as(true)
          .handleErrorWith: error =>
            if NodeError.hasCode(error, "ESRCH") then IO.pure(false)
            else if NodeError.hasCode(error, "EPERM") then IO.pure(true)
            else IO.raiseError(error)
      .handleError(_ => false)

  private def connectSocket(
      transport: WorkspaceChannelTransport,
  ): IO[NodeSocket] =
    IO.async[NodeSocket]: complete =>
      IO.delay:
        var settled = false
        val options = transport match
          case WorkspaceChannelTransport.Unix(path) =>
            js.Dynamic.literal(path = path)
          case WorkspaceChannelTransport.Tcp(host, port) =>
            js.Dynamic.literal(host = host, port = port)
        val socket                        = NodeNet.createConnection(options)
        val connected: js.Function0[Unit] = () =>
          if !settled then
            settled = true
            complete(Right(socket))
        val failed: js.Function1[js.Any, Unit] = raw =>
          if !settled then
            settled = true
            complete(
              Left(
                IllegalStateException(
                  s"could not connect to the workspace IPC endpoint: ${NodeProcess.errorMessage(raw)}",
                ),
              ),
            )
        socket.once("connect", connected)
        socket.once("error", failed)
        Some(
          IO.delay:
            settled = true
            socket.off("connect", connected)
            socket.off("error", failed)
            socket.destroy(),
        )

  private def digest(value: String): String =
    NodeCrypto.createHash("sha256").update(value).digest("hex")

  private def promise[A](value: => js.Promise[A], context: String): IO[A] =
    IO.fromPromise(IO.delay(value))
      .adaptError: error =>
        IllegalStateException(s"$context: ${error.getMessage}", error)

private[infrastructure] object WorkspaceChannelMetadataParser:
  def ownerPid(text: String): Either[String, Int] =
    decode(text).flatMap: raw =>
      integer(raw, "pid").flatMap: pid =>
        require(pid > 0, "IPC metadata pid must be positive").as(pid)

  def parse(
      text: String,
      endpoint: WorkspaceChannelEndpoint,
  ): Either[String, WorkspaceChannelMetadata] =
    decode(text).flatMap: raw =>
      for
        version <- integer(raw, "version")
        _ <- require(version == 1, s"unsupported IPC metadata version $version")
        protocol <- string(raw, "protocol")
        _        <- require(
          protocol == WorkspaceChannel.Protocol,
          s"unsupported IPC protocol '$protocol'",
        )
        kind      <- string(raw, "transport")
        pid       <- integer(raw, "pid")
        _         <- require(pid > 0, "IPC metadata pid must be positive")
        sessionId <- nonEmpty(raw, "sessionId")
        launchId  <- nonEmpty(raw, "launchId")
        token     <- nonEmpty(raw, "token")
        _         <- require(
          token.length >= 32,
          "IPC metadata token is too short",
        )
        transport <- kind match
          case "unix" =>
            string(raw, "address").flatMap: path =>
              val suffix = path.stripPrefix(endpoint.socketPrefix)
              require(
                path.startsWith(endpoint.socketPrefix) &&
                  suffix.length == 21 &&
                  suffix.endsWith(".sock") &&
                  suffix.dropRight(5).forall(isLowerHex),
                "IPC metadata contains an unexpected socket path",
              ).as(WorkspaceChannelTransport.Unix(path))
          case "tcp" =>
            for
              host <- string(raw, "host")
              _ <- require(host == "127.0.0.1", "IPC TCP host must be loopback")
              port <- integer(raw, "port")
              _ <- require(port > 0 && port <= 65535, "IPC TCP port is invalid")
            yield WorkspaceChannelTransport.Tcp(host, port)
          case other => Left(s"unknown IPC transport '$other'")
      yield WorkspaceChannelMetadata(
        transport = transport,
        pid = pid,
        sessionId = sessionId,
        launchId = launchId,
        token = token,
      )

  private def decode(text: String): Either[String, js.Dynamic] =
    try
      val value = js.JSON.parse(text)
      Either.cond(
        value != null && js.typeOf(value) == "object" && !js.Array.isArray(
          value,
        ),
        value.asInstanceOf[js.Dynamic],
        "IPC metadata must be a JSON object",
      )
    catch
      case error: js.JavaScriptException =>
        Left(s"IPC metadata is malformed JSON: ${error.getMessage}")

  private def string(value: js.Dynamic, name: String): Either[String, String] =
    val field = value.selectDynamic(name)
    Either.cond(
      !js.isUndefined(field) && js.typeOf(field) == "string",
      field.asInstanceOf[String],
      s"IPC metadata field '$name' must be a string",
    )

  private def nonEmpty(
      value: js.Dynamic,
      name: String,
  ): Either[String, String] =
    string(value, name).flatMap: result =>
      Either.cond(
        result.nonEmpty,
        result,
        s"IPC metadata field '$name' must not be empty",
      )

  private def integer(value: js.Dynamic, name: String): Either[String, Int] =
    val field = value.selectDynamic(name)
    val valid = !js.isUndefined(field) && js.typeOf(field) == "number" &&
      field.asInstanceOf[Double].isWhole &&
      field.asInstanceOf[Double] >= Int.MinValue &&
      field.asInstanceOf[Double] <= Int.MaxValue
    Either.cond(
      valid,
      field.asInstanceOf[Double].toInt,
      s"IPC metadata field '$name' must be an integer",
    )

  private def require(
      condition: Boolean,
      problem: => String,
  ): Either[String, Unit] =
    Either.cond(condition, (), problem)

  private def isLowerHex(value: Char): Boolean =
    value >= '0' && value <= '9' || value >= 'a' && value <= 'f'

private[infrastructure] final class NdjsonFrameDecoder(maxFrameBytes: Int):
  private var pending = ""

  def offer(chunk: String): Either[String, Vector[String]] =
    val complete   = Vector.newBuilder[String]
    var frameStart = 0
    var problem    = Option.empty[String]
    var newline    = chunk.indexOf('\n', frameStart)
    while newline >= 0 && problem.isEmpty do
      appendBounded(chunk, frameStart, newline) match
        case Left(error) => problem = Some(error)
        case Right(raw)  =>
          pending = ""
          val frame = if raw.endsWith("\r") then raw.dropRight(1) else raw
          if frame.isEmpty then problem = Some("IPC frame must not be empty")
          else complete += frame
      frameStart = newline + 1
      newline = chunk.indexOf('\n', frameStart)

    if problem.isEmpty then
      appendBounded(chunk, frameStart, chunk.length) match
        case Left(error)      => problem = Some(error)
        case Right(remainder) => pending = remainder

    problem.toLeft(complete.result())

  private def utf8Bytes(value: String): Int =
    NodeBuffer.Buffer.byteLength(value, "utf8")

  private def appendBounded(
      chunk: String,
      start: Int,
      end: Int,
  ): Either[String, String] =
    val combinedCharacters = pending.length + end - start
    if combinedCharacters > maxFrameBytes + 1 then frameTooLarge
    else
      val candidate      = pending + chunk.substring(start, end)
      val candidateBytes = utf8Bytes(candidate)
      Either.cond(
        candidateBytes <= maxFrameBytes || candidateBytes == maxFrameBytes + 1 &&
          candidate.endsWith("\r"),
        candidate,
        s"IPC frame exceeds $maxFrameBytes bytes",
      )

  private def frameTooLarge: Left[String, Nothing] =
    Left(s"IPC frame exceeds $maxFrameBytes bytes")

private[infrastructure] enum WorkspaceChannelMessage:
  case Ready(url: PreviewUrl)
  case Pong
  case Stopping
  case Stopped
  case Error(message: String)

private[infrastructure] object WorkspaceChannelProtocol:
  def hello(token: String, clientId: String, requestId: String): String =
    encode(
      Vector(
        "version"   -> "1",
        "type"      -> quoted("hello"),
        "token"     -> quoted(token),
        "clientId"  -> quoted(clientId),
        "requestId" -> quoted(requestId),
      ),
    )

  def stop(token: String, clientId: String, requestId: String): String =
    encode(
      Vector(
        "version"   -> "1",
        "type"      -> quoted("stop"),
        "token"     -> quoted(token),
        "clientId"  -> quoted(clientId),
        "requestId" -> quoted(requestId),
      ),
    )

  def ping(token: String, clientId: String, requestId: String): String =
    encode(
      Vector(
        "version"   -> "1",
        "type"      -> quoted("ping"),
        "token"     -> quoted(token),
        "clientId"  -> quoted(clientId),
        "requestId" -> quoted(requestId),
      ),
    )

  def decodeServer(
      frame: String,
      helloRequestId: String,
      pingRequestId: Option[String],
      stopRequestId: Option[String],
      expectedSessionId: String,
      expectedPid: Int,
      expectedLaunchId: String,
  ): Either[String, WorkspaceChannelMessage] =
    decode(frame).flatMap: raw =>
      for
        version <- integer(raw, "version")
        _    <- require(version == 1, s"unknown IPC protocol version $version")
        kind <- string(raw, "type")
        message <- kind match
          case "ready" =>
            for
              requestId <- string(raw, "requestId")
              _         <- require(
                requestId == helloRequestId,
                "ready response has an unknown request id",
              )
              sessionId <- string(raw, "sessionId")
              _         <- require(
                sessionId == expectedSessionId,
                "ready response has a conflicting session id",
              )
              pid <- integer(raw, "pid")
              _   <- require(
                pid == expectedPid,
                "ready response has a conflicting process id",
              )
              launchId <- string(raw, "launchId")
              _        <- require(
                launchId == expectedLaunchId,
                "ready response has a conflicting launch id",
              )
              text <- string(raw, "url")
              url  <- PreviewUrl.parse(text).leftMap(_.message)
            yield WorkspaceChannelMessage.Ready(url)
          case "pong" =>
            validateOptionalRequest(
              raw,
              pingRequestId,
              "pong has an unknown request id",
            )
              .as(WorkspaceChannelMessage.Pong)
          case "stopping" =>
            validateStopRequest(raw, stopRequestId)
              .as(WorkspaceChannelMessage.Stopping)
          case "stopped" =>
            validateStopRequest(raw, stopRequestId)
              .as(WorkspaceChannelMessage.Stopped)
          case "error" =>
            string(raw, "message").map(WorkspaceChannelMessage.Error(_))
          case other => Left(s"unknown IPC message type '$other'")
      yield message

  private def validateStopRequest(
      raw: js.Dynamic,
      expected: Option[String],
  ): Either[String, Unit] =
    validateOptionalRequest(
      raw,
      expected,
      "stop acknowledgement has an unknown request id",
    )

  private def validateOptionalRequest(
      raw: js.Dynamic,
      expected: Option[String],
      problem: String,
  ): Either[String, Unit] =
    string(raw, "requestId").flatMap: actual =>
      require(expected.contains(actual), problem)

  private def decode(frame: String): Either[String, js.Dynamic] =
    try
      val value = js.JSON.parse(frame)
      Either.cond(
        value != null && js.typeOf(value) == "object" && !js.Array.isArray(
          value,
        ),
        value.asInstanceOf[js.Dynamic],
        "IPC frame must be a JSON object",
      )
    catch
      case error: js.JavaScriptException =>
        Left(s"malformed IPC JSON: ${error.getMessage}")

  private def string(value: js.Dynamic, name: String): Either[String, String] =
    val field = value.selectDynamic(name)
    Either.cond(
      !js.isUndefined(field) && js.typeOf(field) == "string",
      field.asInstanceOf[String],
      s"IPC field '$name' must be a string",
    )

  private def integer(value: js.Dynamic, name: String): Either[String, Int] =
    val field = value.selectDynamic(name)
    val valid = !js.isUndefined(field) && js.typeOf(field) == "number" &&
      field.asInstanceOf[Double].isWhole &&
      field.asInstanceOf[Double] >= Int.MinValue &&
      field.asInstanceOf[Double] <= Int.MaxValue
    Either.cond(
      valid,
      field.asInstanceOf[Double].toInt,
      s"IPC field '$name' must be an integer",
    )

  private def encode(fields: Vector[(String, String)]): String =
    fields
      .map((name, value) => s"${quoted(name)}:$value")
      .mkString("{", ",", "}\n")

  private def quoted(value: String): String =
    js.JSON.stringify(value)

  private def require(
      condition: Boolean,
      problem: => String,
  ): Either[String, Unit] =
    Either.cond(condition, (), problem)

private[infrastructure] object ReadyAnnouncement:
  def validate(
      current: Option[PreviewUrl],
      announced: PreviewUrl,
  ): Either[String, PreviewUrl] =
    current match
      case None                                    => Right(announced)
      case Some(existing) if existing == announced =>
        Left("duplicate ready announcement")
      case Some(existing) =>
        Left(
          s"conflicting ready announcement (${existing.value} versus ${announced.value})",
        )

private[infrastructure] final class WorkspaceChannelConnection private (
    socket: NodeSocket,
    metadata: WorkspaceChannelMetadata,
    clientId: String,
    helloRequestId: String,
    readyResult: Deferred[IO, Either[Throwable, PreviewUrl]],
    pongResult: Deferred[IO, Either[Throwable, Unit]],
    terminalResult: Deferred[IO, ProcessExit],
    pingRequest: Ref[IO, Option[String]],
    stopRequest: Ref[IO, Option[String]],
    listeners: WorkspaceChannelListeners,
):
  def awaitUrl: IO[PreviewUrl] = readyResult.get.flatMap(_.liftTo[IO])

  def awaitExit: IO[ProcessExit] = terminalResult.get

  def ping: IO[Unit] =
    IO
      .delay(NodeCrypto.randomUUID())
      .flatMap: candidate =>
        pingRequest
          .modify:
            case None           => Some(candidate) -> Some(candidate)
            case some @ Some(_) => some            -> None
          .flatMap:
            case Some(requestId) =>
              IO.delay(
                socket.write(
                  WorkspaceChannelProtocol.ping(
                    token = metadata.token,
                    clientId = clientId,
                    requestId = requestId,
                  ),
                ),
              ).void
            case None => IO.unit
        *> pongResult.get.flatMap(_.liftTo[IO])

  def stop: IO[Unit] =
    IO
      .delay(NodeCrypto.randomUUID())
      .flatMap: candidate =>
        stopRequest
          .modify:
            case None           => Some(candidate) -> candidate
            case Some(existing) => Some(existing)  -> existing
          .flatMap: requestId =>
            IO.delay(
              socket.write(
                WorkspaceChannelProtocol.stop(
                  token = metadata.token,
                  clientId = clientId,
                  requestId = requestId,
                ),
              ),
            ).void *> terminalResult.get.flatMap: exit =>
              exit.error match
                case Some(problem) =>
                  IO.raiseError(IllegalStateException(problem))
                case None => IO.unit

  def close: IO[Unit] =
    val problem = IllegalStateException("workspace IPC connection closed")
    IO.delay:
      val ignoreError: js.Function1[js.Any, Unit] = _ => ()
      socket.on("error", ignoreError)
      socket.off("data", listeners.data)
      socket.off("error", listeners.error)
      socket.off("close", listeners.close)
      socket.destroy()
    *> readyResult.complete(Left(problem)).void
      *> pongResult.complete(Left(problem)).void
      *> terminalResult
        .complete(
          ProcessExit(
            code = None,
            signal = None,
            error = Some(problem.getMessage),
          ),
        )
        .void

private[infrastructure] final case class WorkspaceChannelListeners(
    data: js.Function1[String, Unit],
    error: js.Function1[js.Any, Unit],
    close: js.Function0[Unit],
)

private[infrastructure] object WorkspaceChannelConnection:
  def create(
      dispatcher: Dispatcher[IO],
      socket: NodeSocket,
      metadata: WorkspaceChannelMetadata,
  ): IO[WorkspaceChannelConnection] =
    for
      ready       <- Deferred[IO, Either[Throwable, PreviewUrl]]
      pong        <- Deferred[IO, Either[Throwable, Unit]]
      terminal    <- Deferred[IO, ProcessExit]
      pingRequest <- Ref.of[IO, Option[String]](None)
      stopRequest <- Ref.of[IO, Option[String]](None)
      clientId       = NodeCrypto.randomUUID()
      helloRequestId = NodeCrypto.randomUUID()
      decoder        = NdjsonFrameDecoder(WorkspaceChannel.MaxFrameBytes)
      announced <- Ref.of[IO, Option[PreviewUrl]](None)
      lastError <- Ref.of[IO, Option[String]](None)
      fail = (problem: String) =>
        val error = IllegalStateException(problem)
        ready.complete(Left(error)).void *>
          pong.complete(Left(error)).void *>
          terminal
            .complete(
              ProcessExit(code = None, signal = None, error = Some(problem)),
            )
            .void *>
          IO.delay(socket.destroy())
      handle = (frame: String) =>
        (pingRequest.get, stopRequest.get).tupled.flatMap: (pingId, stopId) =>
          IO.fromEither(
            WorkspaceChannelProtocol
              .decodeServer(
                frame = frame,
                helloRequestId = helloRequestId,
                pingRequestId = pingId,
                stopRequestId = stopId,
                expectedSessionId = metadata.sessionId,
                expectedPid = metadata.pid,
                expectedLaunchId = metadata.launchId,
              )
              .leftMap(IllegalStateException(_)),
          ).attempt
            .flatMap:
              case Left(error) => fail(error.getMessage)
              case Right(WorkspaceChannelMessage.Ready(url)) =>
                announced
                  .modify:
                    case current =>
                      ReadyAnnouncement.validate(current, url) match
                        case Right(accepted) =>
                          Some(accepted) -> Right(accepted)
                        case Left(problem) => current -> Left(problem)
                  .flatMap:
                    case Right(accepted) => ready.complete(Right(accepted)).void
                    case Left(problem)   => fail(problem)
              case Right(WorkspaceChannelMessage.Pong) =>
                pong.complete(Right(())).void
              case Right(WorkspaceChannelMessage.Stopping) => IO.unit
              case Right(WorkspaceChannelMessage.Stopped)  =>
                terminal
                  .complete(
                    ProcessExit(code = Some(0), signal = None, error = None),
                  )
                  .void
              case Right(WorkspaceChannelMessage.Error(message)) =>
                fail(message)
      data = (
          (chunk: String) =>
            dispatcher.unsafeRunAndForget(
              decoder.offer(chunk) match
                case Left(problem) => fail(problem)
                case Right(frames) => frames.traverse_(handle),
            ),
      ): js.Function1[String, Unit]
      error = (
          (raw: js.Any) =>
            dispatcher.unsafeRunAndForget(
              lastError.set(Some(NodeProcess.errorMessage(raw))),
            ),
      ): js.Function1[js.Any, Unit]
      close = (
          () =>
            dispatcher.unsafeRunAndForget(
              lastError.get.flatMap: recorded =>
                val problem =
                  recorded.getOrElse("workspace IPC connection ended")
                ready.complete(Left(IllegalStateException(problem))).void *>
                  pong
                    .complete(Left(IllegalStateException(problem)))
                    .void *>
                  terminal
                    .complete(
                      ProcessExit(
                        code = None,
                        signal = None,
                        error = Some(problem),
                      ),
                    )
                    .void,
            ),
      ): js.Function0[Unit]
      listeners = WorkspaceChannelListeners(
        data = data,
        error = error,
        close = close,
      )
      _ <- IO.delay:
        socket.setEncoding("utf8")
        socket.on("data", data)
        socket.on("error", error)
        socket.once("close", close)
        socket.write(
          WorkspaceChannelProtocol.hello(
            token = metadata.token,
            clientId = clientId,
            requestId = helloRequestId,
          ),
        )
    yield WorkspaceChannelConnection(
      socket = socket,
      metadata = metadata,
      clientId = clientId,
      helloRequestId = helloRequestId,
      readyResult = ready,
      pongResult = pong,
      terminalResult = terminal,
      pingRequest = pingRequest,
      stopRequest = stopRequest,
      listeners = listeners,
    )

private[infrastructure] object WorkspacePreview:
  def launch(
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
  )(
      workspace: Workspace,
      command: PreviewCommand,
  ): Resource[IO, RunningProcess[IO]] =
    command.endpoint match
      case PreviewEndpoint.Fixed(_) =>
        NodeProcess.launch(dispatcher, append)(workspace, command)
      case PreviewEndpoint.Announced =>
        Resource
          .eval(WorkspaceChannel.endpoint(workspace))
          .flatMap: endpoint =>
            Resource
              .make(acquire(dispatcher, append, endpoint, workspace, command))(
                _.release,
              )
              .map(_.running)

  private def acquire(
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
      endpoint: WorkspaceChannelEndpoint,
      workspace: Workspace,
      command: PreviewCommand,
  ): IO[DynamicPreviewRuntime] =
    WorkspaceChannel
      .endpointOwnerAlive(endpoint)
      .flatMap:
        case true =>
          val connect = reusableConnection(
            dispatcher = dispatcher,
            endpoint = endpoint,
          ).flatTap(_ =>
            append(
              s"[${workspace.name}] Reusing the existing workspace preview as a read-only lease.\n",
            ).handleError(_ => ()),
          )
          DynamicPreviewRuntime.reused(connect)
        case false =>
          launchOwned(dispatcher, append, endpoint, workspace, command)

  private def launchOwned(
      dispatcher: Dispatcher[IO],
      append: String => IO[Unit],
      endpoint: WorkspaceChannelEndpoint,
      workspace: Workspace,
      command: PreviewCommand,
  ): IO[DynamicPreviewRuntime] =
    val launchId = NodeCrypto.randomUUID()
    IO.uncancelable: poll =>
      poll(
        NodeProcess
          .launchWithEnvironment(
            dispatcher = dispatcher,
            append = append,
            environment = endpoint.environment(launchId),
          )(workspace, command)
          .allocated,
      ).flatMap: (process, release) =>
        val capture = Ref
          .of[IO, DynamicPreviewConnection](
            DynamicPreviewConnection.Pending,
          )
          .map(connections =>
            DynamicPreviewRuntime.owned(
              dispatcher = dispatcher,
              endpoint = endpoint,
              launchId = launchId,
              process = process,
              releaseProcess = release,
              connection = connections,
              append = append,
              workspaceName = workspace.name,
            ),
          )
        poll(capture)
          .onCancel(release)
          .onError(_ => release.attempt.void)

  private def reusableConnection(
      dispatcher: Dispatcher[IO],
      endpoint: WorkspaceChannelEndpoint,
  ): IO[WorkspaceChannelConnection] =
    def connect: IO[WorkspaceChannelConnection] =
      WorkspaceChannel
        .connectReady(
          dispatcher = dispatcher,
          endpoint = endpoint,
          verifyPing = true,
          expectedLaunchId = None,
        )
        .handleErrorWith: error =>
          WorkspaceChannel
            .endpointOwnerAlive(endpoint)
            .flatMap:
              case true  => IO.sleep(50.millis) *> connect
              case false => IO.raiseError(error)

    connect

private[infrastructure] final class DynamicPreviewRuntime private (
    val running: RunningProcess[IO],
    val release: IO[Unit],
)

private[infrastructure] object DynamicPreviewShutdown:
  def release(
      gracefulStop: IO[Unit],
      awaitProcessExit: IO[Unit],
      closeChannel: IO[Unit],
      releaseProcess: IO[Unit],
      timeout: FiniteDuration,
      onFallback: Throwable => IO[Unit],
  ): IO[Unit] =
    val channelCleanup = (gracefulStop *> awaitProcessExit)
      .timeout(timeout)
      .handleErrorWith(error => onFallback(error).attempt.void) *>
      closeChannel *>
      IO.unit
    for
      channelResult <- channelCleanup.attempt
      processResult <- releaseProcess.attempt
      _             <- (channelResult, processResult) match
        case (Right(_), Right(_))           => IO.unit
        case (Left(channelError), Right(_)) => IO.raiseError(channelError)
        case (Right(_), Left(processError)) => IO.raiseError(processError)
        case (Left(channelError), Left(processError)) =>
          IO.delay(processError.addSuppressed(channelError)) *>
            IO.raiseError(processError)
    yield ()

private[infrastructure] object DynamicPreviewRuntime:
  def reused(
      acquire: IO[WorkspaceChannelConnection],
  ): IO[DynamicPreviewRuntime] =
    for
      shared     <- acquire.memoize
      connection <- Ref.of[IO, DynamicPreviewConnection](
        DynamicPreviewConnection.Pending,
      )
      stop <- Deferred[IO, Unit]
    yield
      def connected: IO[WorkspaceChannelConnection] =
        connection.get.flatMap:
          case DynamicPreviewConnection.Active(value) => IO.pure(value)
          case DynamicPreviewConnection.Releasing     =>
            IO.raiseError(
              IllegalStateException("the reused preview is closing"),
            )
          case DynamicPreviewConnection.Pending =>
            acquireAndInstall(
              connection,
              shared,
            ).race(stop.get)
              .flatMap:
                case Left(value) => IO.pure(value)
                case Right(_)    =>
                  IO.raiseError(
                    IllegalStateException("the reused preview was closed"),
                  )

      val release = stop.complete(()).void *>
        connection
          .modify:
            case DynamicPreviewConnection.Active(active) =>
              DynamicPreviewConnection.Releasing -> Some(active)
            case DynamicPreviewConnection.Pending |
                DynamicPreviewConnection.Releasing =>
              DynamicPreviewConnection.Releasing -> None
          .flatMap(_.traverse_(_.close))

      new DynamicPreviewRuntime(
        running = RunningProcess.announced(
          awaitExit = connected.flatMap(_.awaitExit),
          awaitUrl = connected.flatMap(_.awaitUrl),
        ),
        release = release,
      )

  private[infrastructure] def installConnection(
      connection: Ref[IO, DynamicPreviewConnection],
      value: WorkspaceChannelConnection,
  ): IO[WorkspaceChannelConnection] =
    connection
      .modify:
        case DynamicPreviewConnection.Pending =>
          DynamicPreviewConnection.Active(value) ->
            DynamicConnectionInstall.Installed(value)
        case current @ DynamicPreviewConnection.Active(existing) =>
          current -> DynamicConnectionInstall.Existing(
            existing = existing,
            redundant = value,
          )
        case DynamicPreviewConnection.Releasing =>
          DynamicPreviewConnection.Releasing ->
            DynamicConnectionInstall.Rejected(value)
      .flatMap:
        case DynamicConnectionInstall.Installed(installed) =>
          IO.pure(installed)
        case DynamicConnectionInstall.Existing(existing, redundant)
            if existing.eq(redundant) =>
          IO.pure(existing)
        case DynamicConnectionInstall.Existing(existing, redundant) =>
          redundant.close.as(existing)
        case DynamicConnectionInstall.Rejected(rejected) =>
          rejected.close *>
            IO.raiseError(
              IllegalStateException(
                "the preview stopped during IPC connection",
              ),
            )

  private def acquireAndInstall(
      connection: Ref[IO, DynamicPreviewConnection],
      acquire: IO[WorkspaceChannelConnection],
  ): IO[WorkspaceChannelConnection] =
    Ref
      .of[IO, Boolean](false)
      .flatMap: transferred =>
        Resource
          .make(acquire)(value => transferred.get.ifM(IO.unit, value.close))
          .use: value =>
            IO.uncancelable: _ =>
              installConnection(connection, value).flatTap(_ =>
                transferred.set(true),
              )

  def owned(
      dispatcher: Dispatcher[IO],
      endpoint: WorkspaceChannelEndpoint,
      launchId: String,
      process: RunningProcess[IO],
      releaseProcess: IO[Unit],
      connection: Ref[IO, DynamicPreviewConnection],
      append: String => IO[Unit],
      workspaceName: String,
  ): DynamicPreviewRuntime =
    def connected: IO[WorkspaceChannelConnection] =
      connection.get.flatMap:
        case DynamicPreviewConnection.Active(value) => IO.pure(value)
        case DynamicPreviewConnection.Releasing     =>
          IO.raiseError(
            IllegalStateException("the dynamic preview is stopping"),
          )
        case DynamicPreviewConnection.Pending =>
          acquireAndInstall(
            connection,
            WorkspaceChannel.connectReady(
              dispatcher = dispatcher,
              endpoint = endpoint,
              verifyPing = true,
              expectedLaunchId = Some(launchId),
            ),
          ).attempt
            .flatMap:
              case Right(value)                         => IO.pure(value)
              case Left(error: WorkspaceLaunchMismatch) =>
                WorkspaceChannel
                  .endpointOwnerAlive(endpoint)
                  .flatMap:
                    case true  => IO.raiseError(error)
                    case false => IO.sleep(50.millis) *> connected
              case Left(error: WorkspaceMetadataFailure)
                  if !error.retryableDuringPublication =>
                IO.raiseError(error)
              case Left(_) => IO.sleep(50.millis) *> connected

    val release = connection
      .modify:
        case DynamicPreviewConnection.Active(active) =>
          DynamicPreviewConnection.Releasing -> Some(active)
        case DynamicPreviewConnection.Pending |
            DynamicPreviewConnection.Releasing =>
          DynamicPreviewConnection.Releasing -> None
      .flatMap:
        case Some(active) =>
          DynamicPreviewShutdown.release(
            gracefulStop = active.stop,
            awaitProcessExit = process.awaitExit.void,
            closeChannel = active.close,
            releaseProcess = releaseProcess,
            timeout = 3.seconds,
            onFallback = error =>
              append(
                s"[$workspaceName] Graceful IPC stop failed (${error.getMessage}); using exact process-tree cleanup.\n",
              ).handleError(_ => ()),
          )
        case None => releaseProcess

    new DynamicPreviewRuntime(
      running = RunningProcess.announced(
        awaitExit = process.awaitExit,
        awaitUrl = connected.flatMap(_.awaitUrl),
      ),
      release = release,
    )

private[infrastructure] enum DynamicPreviewConnection:
  case Pending
  case Active(connection: WorkspaceChannelConnection)
  case Releasing

private[infrastructure] enum DynamicConnectionInstall:
  case Installed(connection: WorkspaceChannelConnection)
  case Existing(
      existing: WorkspaceChannelConnection,
      redundant: WorkspaceChannelConnection,
  )
  case Rejected(connection: WorkspaceChannelConnection)
