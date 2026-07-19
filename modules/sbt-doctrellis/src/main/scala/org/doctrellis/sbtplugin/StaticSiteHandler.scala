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
    publicationLock.read(handleLocked(exchange))

  private def handleLocked(exchange: HttpExchange): Unit =
    val method = exchange.getRequestMethod
    if method != "GET" && method != "HEAD" then
      exchange.getResponseHeaders.set("Allow", "GET, HEAD")
      sendText(exchange, 405, "Method Not Allowed")
    else
      StaticSiteHandler.resolve(
        normalizedRoot,
        exchange.getRequestURI.getRawPath,
      ) match
        case Some(file) if Files.isRegularFile(file) =>
          val size = Files.size(file)
          exchange.getResponseHeaders.set(
            "Content-Type",
            StaticSiteHandler.contentType(file),
          )
          exchange.getResponseHeaders.set("Cache-Control", "no-store")
          if method == "HEAD" then
            exchange.getResponseHeaders.set("Content-Length", size.toString)
            exchange.sendResponseHeaders(200, -1)
            exchange.close()
          else if size == 0L then
            exchange.getResponseHeaders.set("Content-Length", "0")
            exchange.sendResponseHeaders(200, -1)
            exchange.close()
          else
            exchange.sendResponseHeaders(200, size)
            val output = exchange.getResponseBody
            try Files.copy(file, output): Unit
            finally output.close()
        case _ => sendText(exchange, 404, "Not Found")

  private def sendText(
      exchange: HttpExchange,
      status: Int,
      message: String,
  ): Unit =
    val bytes = message.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", "text/plain; charset=utf-8")
    if exchange.getRequestMethod == "HEAD" then
      exchange.getResponseHeaders.set("Content-Length", bytes.length.toString)
      exchange.sendResponseHeaders(status, -1)
      exchange.close()
    else
      exchange.sendResponseHeaders(status, bytes.length)
      val output = exchange.getResponseBody
      try output.write(bytes)
      finally output.close()

private[sbtplugin] object StaticSiteHandler:
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
