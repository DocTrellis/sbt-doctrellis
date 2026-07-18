package org.doctrellis.sbtplugin

import com.sun.net.httpserver.{HttpExchange, HttpHandler}

import java.io.IOException
import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

private[sbtplugin] final class StaticSiteHandler private (root: Path)
    extends HttpHandler:
  private val normalizedRoot = root.toAbsolutePath.normalize()

  override def handle(exchange: HttpExchange): Unit =
    val method = exchange.getRequestMethod
    if method != "GET" && method != "HEAD" then
      sendText(exchange, 405, "Method Not Allowed")
    else
      StaticSiteHandler.resolve(
        normalizedRoot,
        exchange.getRequestURI.getPath,
      ) match
        case Some(file) if Files.isRegularFile(file) =>
          val size = Files.size(file)
          exchange.getResponseHeaders.set(
            "Content-Type",
            StaticSiteHandler.contentType(file),
          )
          exchange.getResponseHeaders.set("Cache-Control", "no-store")
          if method == "HEAD" then
            exchange.sendResponseHeaders(200, -1)
            exchange.close()
          else
            exchange.sendResponseHeaders(200, size)
            val output = exchange.getResponseBody
            try Files.copy(file, output)
            finally output.close()
        case _ => sendText(exchange, 404, "Not Found")

  private def sendText(
      exchange: HttpExchange,
      status: Int,
      message: String,
  ): Unit =
    val bytes = message.getBytes(StandardCharsets.UTF_8)
    exchange.getResponseHeaders.set("Content-Type", "text/plain; charset=utf-8")
    exchange.sendResponseHeaders(status, bytes.length)
    val output = exchange.getResponseBody
    try output.write(bytes)
    finally output.close()

private[sbtplugin] object StaticSiteHandler:
  def apply(root: Path): StaticSiteHandler = new StaticSiteHandler(root)

  def resolve(root: Path, requestPath: String): Option[Path] =
    for
      decoded  <- decode(requestPath)
      realRoot <- realPath(root)
      relative  = decoded.stripPrefix("/")
      candidate = realRoot.resolve(relative).normalize()
      page      =
        if Files.isDirectory(candidate) then candidate.resolve("index.html")
        else candidate
      realPage <- Option.when(page.startsWith(realRoot))(page).flatMap(realPath)
      if realPage.startsWith(realRoot)
    yield realPage

  private def decode(requestPath: String): Option[String] =
    try
      Some(
        URLDecoder.decode(
          Option(requestPath).getOrElse("/"),
          StandardCharsets.UTF_8,
        ),
      )
    catch case _: IllegalArgumentException => None

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
