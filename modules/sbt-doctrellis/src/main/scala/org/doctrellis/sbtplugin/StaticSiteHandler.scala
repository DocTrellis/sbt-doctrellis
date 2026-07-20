package org.doctrellis.sbtplugin

import java.io.{IOException, InputStream, OutputStream}
import java.nio.ByteBuffer
import java.nio.channels.{Channels, FileChannel}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, InvalidPathException, Path, StandardOpenOption}

import scala.collection.mutable.ArrayBuilder
import scala.util.Using

import com.sun.net.httpserver.{HttpExchange, HttpHandler}

private[sbtplugin] final class StaticSiteHandler private (
    root: Path,
    publicationLock: PreviewPublicationLock,
    maximumInMemoryResponseBytes: Long,
    createTemporaryFile: () => Path,
    deleteTemporaryFile: Path => Unit,
) extends HttpHandler:
  private val normalizedRoot = root.toAbsolutePath.normalize()
  require(
    maximumInMemoryResponseBytes >= 0L &&
      maximumInMemoryResponseBytes < Int.MaxValue.toLong,
    s"DocTrellis maximum in-memory response bytes must be between 0 and ${Int.MaxValue.toLong - 1L}.",
  )

  override def handle(exchange: HttpExchange): Unit =
    val method   = exchange.getRequestMethod
    val response =
      try
        publicationLock.read(
          snapshot(method, exchange.getRequestURI.getRawPath),
        )
      catch
        case error: Throwable =>
          throw StaticSiteHandler.close(exchange, error)
    var failure: Throwable = null
    try send(exchange, response)
    catch case error: Throwable => failure = error
    failure = StaticSiteHandler.close(response, failure)
    failure = StaticSiteHandler.close(exchange, failure)
    if failure != null then throw failure

  private def snapshot(
      method: String,
      requestPath: String,
  ): StaticSiteHandler.ResponseSnapshot =
    if method != "GET" && method != "HEAD" then
      StaticSiteHandler.textResponse(
        405,
        "Method Not Allowed",
        includeBody = true,
        headers = List("Allow" -> "GET, HEAD"),
      )
    else
      StaticSiteHandler.resolve(
        normalizedRoot,
        requestPath,
      ) match
        case Some(file) if Files.isRegularFile(file) =>
          val contentType = StaticSiteHandler.contentType(file)
          val headers     = List(
            "Content-Type"  -> contentType,
            "Cache-Control" -> "no-store",
          )
          if method == "GET" then
            val body = StaticSiteHandler.snapshotBody(
              file,
              maximumInMemoryResponseBytes,
              createTemporaryFile,
              deleteTemporaryFile,
            )
            try
              StaticSiteHandler.ResponseSnapshot(
                status = 200,
                headers = headers,
                contentLength = body.length,
                body = Some(body),
              )
            catch
              case error: Throwable =>
                throw StaticSiteHandler.close(body, error)
          else
            StaticSiteHandler.ResponseSnapshot(
              status = 200,
              headers = headers,
              contentLength = Files.size(file),
              body = None,
            )
        case _ =>
          StaticSiteHandler.textResponse(
            404,
            "Not Found",
            includeBody = method == "GET",
          )

  private def send(
      exchange: HttpExchange,
      response: StaticSiteHandler.ResponseSnapshot,
  ): Unit =
    response.headers.foreach: (name, value) =>
      exchange.getResponseHeaders.set(name, value)
    response.body match
      case Some(body) if body.length > 0L =>
        exchange.sendResponseHeaders(response.status, response.contentLength)
        val output             = exchange.getResponseBody
        var failure: Throwable = null
        try body.writeTo(output)
        catch case error: Throwable => failure = error
        failure = StaticSiteHandler.close(output, failure)
        if failure != null then throw failure
      case _ =>
        exchange.getResponseHeaders.set(
          "Content-Length",
          response.contentLength.toString,
        )
        exchange.sendResponseHeaders(response.status, -1)
        exchange.close()

private[sbtplugin] object StaticSiteHandler:
  private val MaximumInMemoryResponseBytes = 1024L * 1024L

  private trait ResponseBody extends AutoCloseable:
    def length: Long
    def writeTo(output: OutputStream): Unit

  private final class InMemoryBody(bytes: Array[Byte]) extends ResponseBody:
    override val length: Long                        = bytes.length.toLong
    override def writeTo(output: OutputStream): Unit = output.write(bytes)
    override def close(): Unit                       = ()

  private final class TemporaryFileBody(
      path: Path,
      input: InputStream,
      override val length: Long,
      deleteTemporaryFile: Path => Unit,
  ) extends ResponseBody:
    override def writeTo(output: OutputStream): Unit =
      val transferred = input.transferTo(output)
      if transferred != length then
        throw IOException(
          s"DocTrellis sent $transferred bytes from a $length-byte response snapshot.",
        )

    override def close(): Unit =
      var failure: Throwable = null
      failure = StaticSiteHandler.close(input, failure)
      try deleteTemporaryFile(path)
      catch
        case cleanupError: Throwable =>
          if failure == null then failure = cleanupError
          else if cleanupError ne failure then
            failure.addSuppressed(cleanupError)
      if failure != null then throw failure

  private final case class ResponseSnapshot(
      status: Int,
      headers: List[(String, String)],
      contentLength: Long,
      body: Option[ResponseBody],
  ) extends AutoCloseable:
    override def close(): Unit = body match
      case Some(value) => value.close()
      case None        => ()

  private def textResponse(
      status: Int,
      message: String,
      includeBody: Boolean,
      headers: List[(String, String)] = Nil,
  ): ResponseSnapshot =
    val bytes = message.getBytes(StandardCharsets.UTF_8)
    ResponseSnapshot(
      status,
      "Content-Type" -> "text/plain; charset=utf-8" :: headers,
      bytes.length.toLong,
      Option.when(includeBody)(new InMemoryBody(bytes)),
    )

  private def snapshotBody(
      file: Path,
      maximumInMemoryResponseBytes: Long,
      createTemporaryFile: () => Path,
      deleteTemporaryFile: Path => Unit,
  ): ResponseBody =
    val bytes = Using.resource(Files.newInputStream(file)): input =>
      input.readNBytes(maximumInMemoryResponseBytes.toInt + 1)
    if bytes.length.toLong <= maximumInMemoryResponseBytes then
      new InMemoryBody(bytes)
    else temporaryFileBody(file, createTemporaryFile, deleteTemporaryFile)

  private def temporaryFileBody(
      file: Path,
      createTemporaryFile: () => Path,
      deleteTemporaryFile: Path => Unit,
  ): ResponseBody =
    val temporary            = createTemporaryFile()
    var channel: FileChannel = null
    try
      channel = FileChannel.open(
        temporary,
        StandardOpenOption.READ,
        StandardOpenOption.WRITE,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.DELETE_ON_CLOSE,
      )
      val copied = Using.resource(Files.newInputStream(file)): source =>
        source.transferTo(Channels.newOutputStream(channel))
      val length = channel.size()
      if copied != length then
        throw IOException(
          s"DocTrellis copied $copied bytes into a $length-byte response snapshot.",
        )
      channel.position(0L)
      val body = new TemporaryFileBody(
        temporary,
        Channels.newInputStream(channel),
        length,
        deleteTemporaryFile,
      )
      channel = null
      body
    catch
      case error: Throwable =>
        if channel != null then
          try channel.close()
          catch
            case cleanupError: Throwable =>
              if cleanupError ne error then error.addSuppressed(cleanupError)
        try deleteTemporaryFile(temporary)
        catch
          case cleanupError: Throwable =>
            if cleanupError ne error then error.addSuppressed(cleanupError)
        throw error

  private def close(
      resource: AutoCloseable,
      primary: Throwable,
  ): Throwable =
    try
      resource.close()
      primary
    catch
      case cleanupError: Throwable =>
        if primary == null then cleanupError
        else
          if cleanupError ne primary then primary.addSuppressed(cleanupError)
          primary

  def apply(root: Path): StaticSiteHandler =
    apply(root, PreviewPublicationLock())

  def apply(
      root: Path,
      publicationLock: PreviewPublicationLock,
  ): StaticSiteHandler =
    new StaticSiteHandler(
      root,
      publicationLock,
      MaximumInMemoryResponseBytes,
      () =>
        Files.createTempFile(
          "doctrellis-preview-response-",
          ".snapshot",
        ),
      StaticSiteHandler.deleteTemporaryFile,
    )

  def apply(
      root: Path,
      publicationLock: PreviewPublicationLock,
      maximumInMemoryResponseBytes: Long,
      createTemporaryFile: () => Path,
  ): StaticSiteHandler =
    apply(
      root,
      publicationLock,
      maximumInMemoryResponseBytes,
      createTemporaryFile,
      StaticSiteHandler.deleteTemporaryFile,
    )

  def apply(
      root: Path,
      publicationLock: PreviewPublicationLock,
      maximumInMemoryResponseBytes: Long,
      createTemporaryFile: () => Path,
      deleteTemporaryFile: Path => Unit,
  ): StaticSiteHandler =
    new StaticSiteHandler(
      root,
      publicationLock,
      maximumInMemoryResponseBytes,
      createTemporaryFile,
      deleteTemporaryFile,
    )

  private def deleteTemporaryFile(path: Path): Unit =
    Files.deleteIfExists(path): Unit

  def resolve(root: Path, requestPath: String): Option[Path] =
    try
      for
        decoded  <- decode(requestPath)
        realRoot <- realPath(root)
        relative  = decoded.stripPrefix("/")
        candidate = realRoot.resolve(relative).normalize()
        page      =
          if Files.isDirectory(candidate) then candidate.resolve("index.html")
          else candidate
        realPage <- Option
          .when(page.startsWith(realRoot))(page)
          .flatMap(realPath)
        if realPage.startsWith(realRoot)
      yield realPage
    catch case _: InvalidPathException => None

  private def decode(requestPath: String): Option[String] =
    val raw     = Option(requestPath).getOrElse("/")
    val decoded = StringBuilder()
    var offset  = 0
    var valid   = true
    try
      while valid && offset < raw.length do
        if raw.charAt(offset) != '%' then
          decoded.append(raw.charAt(offset))
          offset += 1
        else
          val bytes = ArrayBuilder.make[Byte]
          while valid && offset < raw.length && raw.charAt(offset) == '%' do
            if offset + 2 >= raw.length then valid = false
            else
              val high = Character.digit(raw.charAt(offset + 1), 16)
              val low  = Character.digit(raw.charAt(offset + 2), 16)
              if high < 0 || low < 0 then valid = false
              else
                bytes += ((high << 4) | low).toByte
                offset += 3
          if valid then
            val decoder = StandardCharsets.UTF_8
              .newDecoder()
              .onMalformedInput(java.nio.charset.CodingErrorAction.REPORT)
              .onUnmappableCharacter(java.nio.charset.CodingErrorAction.REPORT)
            decoded.append(decoder.decode(ByteBuffer.wrap(bytes.result())))
      Option.when(valid)(decoded.result())
    catch case _: java.nio.charset.CharacterCodingException => None

  private def realPath(path: Path): Option[Path] =
    try Some(path.toAbsolutePath.normalize().toRealPath())
    catch case _: IOException => None

  def contentType(path: Path): String =
    val name = path.getFileName.toString.toLowerCase(java.util.Locale.ROOT)
    if name.endsWith(".html") then "text/html; charset=utf-8"
    else if name.endsWith(".css") then "text/css; charset=utf-8"
    else if name.endsWith(".js") then "text/javascript; charset=utf-8"
    else if name.endsWith(".json") then "application/json; charset=utf-8"
    else if name.endsWith(".svg") then "image/svg+xml"
    else if name.endsWith(".png") then "image/png"
    else if name.endsWith(".jpg") || name.endsWith(".jpeg") then "image/jpeg"
    else if name.endsWith(".woff2") then "font/woff2"
    else if name.endsWith(".woff") then "font/woff"
    else
      Option(Files.probeContentType(path)).getOrElse("application/octet-stream")
