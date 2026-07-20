package org.doctrellis.sbtplugin

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, InvalidPathException, Path}

import scala.collection.mutable.ArrayBuilder

import com.sun.net.httpserver.{HttpExchange, HttpHandler}

private[sbtplugin] final class StaticSiteHandler private (
    root: Path,
    publicationLock: PreviewPublicationLock,
) extends HttpHandler:
  private val normalizedRoot = root.toAbsolutePath.normalize()

  override def handle(exchange: HttpExchange): Unit =
    val method   = exchange.getRequestMethod
    val response = publicationLock.read(
      snapshot(method, exchange.getRequestURI.getRawPath),
    )
    send(exchange, response)

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
          val body = Option.when(method == "GET")(Files.readAllBytes(file))
          StaticSiteHandler.ResponseSnapshot(
            status = 200,
            headers = List(
              "Content-Type"  -> StaticSiteHandler.contentType(file),
              "Cache-Control" -> "no-store",
            ),
            contentLength = body.fold(Files.size(file))(_.length.toLong),
            body = body,
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
      case Some(body) if body.nonEmpty =>
        exchange.sendResponseHeaders(response.status, response.contentLength)
        val output = exchange.getResponseBody
        try output.write(body)
        finally output.close()
      case _ =>
        exchange.getResponseHeaders.set(
          "Content-Length",
          response.contentLength.toString,
        )
        exchange.sendResponseHeaders(response.status, -1)
        exchange.close()

private[sbtplugin] object StaticSiteHandler:
  private final case class ResponseSnapshot(
      status: Int,
      headers: List[(String, String)],
      contentLength: Long,
      body: Option[Array[Byte]],
  )

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
      Option.when(includeBody)(bytes),
    )

  def apply(root: Path): StaticSiteHandler =
    new StaticSiteHandler(root, PreviewPublicationLock())

  def apply(
      root: Path,
      publicationLock: PreviewPublicationLock,
  ): StaticSiteHandler = new StaticSiteHandler(root, publicationLock)

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
