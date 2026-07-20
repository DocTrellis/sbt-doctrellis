package org.doctrellis.sbtplugin

import java.io.{
  ByteArrayInputStream,
  ByteArrayOutputStream,
  InputStream,
  OutputStream,
}
import java.net.{InetSocketAddress, URI}
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.file.Files
import java.util.concurrent.{Callable, CountDownLatch, Executors, TimeUnit}

import com.sun.net.httpserver.{
  Headers,
  HttpContext,
  HttpExchange,
  HttpPrincipal,
}

class StaticSiteHandlerSuite extends munit.FunSuite:
  test("resolves the root and nested directories to index.html"):
    val root = Files.createTempDirectory("doctrellis-handler")
    try
      val nested = Files.createDirectories(root.resolve("guide"))
      Files.writeString(root.resolve("index.html"), "root")
      Files.writeString(nested.resolve("index.html"), "nested")

      assertEquals(
        StaticSiteHandler.resolve(root, "/"),
        Some(root.resolve("index.html").toRealPath()),
      )
      assertEquals(
        StaticSiteHandler.resolve(root, "/guide/"),
        Some(nested.resolve("index.html").toRealPath()),
      )
    finally GeneratedDirectory.delete(root)

  test("does not resolve traversal outside the site root"):
    val root = Files.createTempDirectory("doctrellis-handler")
    try
      assertEquals(StaticSiteHandler.resolve(root, "/../secret.txt"), None)
      assertEquals(
        StaticSiteHandler.resolve(root, "/%2e%2e/secret.txt"),
        None,
      )
      assertEquals(StaticSiteHandler.resolve(root, "/%zz"), None)
      assertEquals(StaticSiteHandler.resolve(root, "/%00"), None)

      val nulExchange = BlockingExchange(
        CountDownLatch(0),
        CountDownLatch(0),
        URI.create("/%00"),
      )
      StaticSiteHandler(root).handle(nulExchange)
      assertEquals(nulExchange.getResponseCode, 404)
    finally GeneratedDirectory.delete(root)

  test("percent-decodes HTTP paths without treating plus as a space"):
    val root = Files.createTempDirectory("doctrellis-handler-percent")
    try
      val literalPlus  = Files.writeString(root.resolve("C++.html"), "plus")
      val plusAndSpace = Files.writeString(
        root.resolve("C++ guide.html"),
        "plus and space",
      )
      Files.writeString(root.resolve("%2B.html"), "decoded once")

      assertEquals(
        StaticSiteHandler.resolve(root, "/C++.html"),
        Some(literalPlus.toRealPath()),
      )
      assertEquals(
        StaticSiteHandler.resolve(root, "/C%2B%2B%20guide.html"),
        Some(plusAndSpace.toRealPath()),
      )
      assertEquals(StaticSiteHandler.resolve(root, "/%c3%28"), None)

      val plusExchange = BlockingExchange(
        CountDownLatch(0),
        CountDownLatch(0),
        URI.create("/C++.html"),
      )
      StaticSiteHandler(root).handle(plusExchange)
      assertEquals(plusExchange.getResponseCode, 200)

      val singleDecodeExchange = BlockingExchange(
        CountDownLatch(0),
        CountDownLatch(0),
        URI.create("/%252B.html"),
      )
      StaticSiteHandler(root).handle(singleDecodeExchange)
      assertEquals(singleDecodeExchange.getResponseCode, 200)
    finally GeneratedDirectory.delete(root)

  test("does not follow site symlinks outside the root"):
    val root    = Files.createTempDirectory("doctrellis-handler")
    val outside = Files.createTempFile("doctrellis-secret", ".txt")
    try
      Files.createSymbolicLink(root.resolve("secret.txt"), outside)
      assertEquals(StaticSiteHandler.resolve(root, "/secret.txt"), None)
    finally
      GeneratedDirectory.delete(root)
      Files.deleteIfExists(outside): Unit

  test("HEAD returns metadata without opening a response body"):
    val root = Files.createTempDirectory("doctrellis-handler-head")
    try
      val body = "published site"
      Files.writeString(root.resolve("index.html"), body)

      val existing = BlockingExchange(
        CountDownLatch(0),
        CountDownLatch(0),
        requestMethod = "HEAD",
        failOnResponseBody = true,
      )
      StaticSiteHandler(root).handle(existing)
      assertEquals(existing.getResponseCode, 200)
      assertEquals(
        existing.getResponseHeaders.getFirst("Content-Length"),
        body.getBytes(java.nio.charset.StandardCharsets.UTF_8).length.toString,
      )
      assertEquals(existing.sentResponseLength, -1L)
      assert(!existing.responseBodyRequested)
      assert(existing.exchangeClosed)

      val missing = BlockingExchange(
        CountDownLatch(0),
        CountDownLatch(0),
        URI.create("/missing.html"),
        requestMethod = "HEAD",
        failOnResponseBody = true,
      )
      StaticSiteHandler(root).handle(missing)
      assertEquals(missing.getResponseCode, 404)
      assertEquals(
        missing.getResponseHeaders.getFirst("Content-Length"),
        "Not Found"
          .getBytes(java.nio.charset.StandardCharsets.UTF_8)
          .length
          .toString,
      )
      assertEquals(missing.sentResponseLength, -1L)
      assert(!missing.responseBodyRequested)
      assert(missing.exchangeClosed)
    finally GeneratedDirectory.delete(root)

  test("405 advertises the supported methods"):
    val root = Files.createTempDirectory("doctrellis-handler-method")
    try
      val exchange = BlockingExchange(
        CountDownLatch(0),
        CountDownLatch(0),
        requestMethod = "POST",
      )

      StaticSiteHandler(root).handle(exchange)

      assertEquals(exchange.getResponseCode, 405)
      assertEquals(exchange.getResponseHeaders.getFirst("Allow"), "GET, HEAD")
    finally GeneratedDirectory.delete(root)

  test("empty GET has an exact zero length without opening a response body"):
    val root = Files.createTempDirectory("doctrellis-handler-empty")
    try
      Files.createFile(root.resolve("empty.html"))
      val exchange = BlockingExchange(
        CountDownLatch(0),
        CountDownLatch(0),
        URI.create("/empty.html"),
        failOnResponseBody = true,
      )

      StaticSiteHandler(root).handle(exchange)

      assertEquals(exchange.getResponseCode, 200)
      assertEquals(exchange.getResponseHeaders.getFirst("Content-Length"), "0")
      assertEquals(exchange.sentResponseLength, -1L)
      assert(!exchange.responseBodyRequested)
      assert(exchange.exchangeClosed)
    finally GeneratedDirectory.delete(root)

  test("the JDK server preserves response metadata for GET, HEAD, and 405"):
    val root   = Files.createTempDirectory("doctrellis-handler-http")
    val server = com.sun.net.httpserver.HttpServer.create(
      InetSocketAddress("127.0.0.1", 0),
      0,
    )
    try
      Files.createFile(root.resolve("empty.html"))
      val nonEmptyBody = "published site"
      Files.writeString(root.resolve("non-empty.html"), nonEmptyBody)
      server.createContext("/", StaticSiteHandler(root))
      server.start()
      val baseUri = URI.create(
        s"http://127.0.0.1:${server.getAddress.getPort}/empty.html",
      )
      val client = HttpClient.newHttpClient()

      val empty = client.send(
        HttpRequest.newBuilder(baseUri).GET().build(),
        HttpResponse.BodyHandlers.ofByteArray(),
      )
      assertEquals(empty.statusCode(), 200)
      assertEquals(
        empty.headers().firstValue("Content-Length").orElse(null),
        "0",
      )
      assertEquals(empty.body().toSeq, Seq.empty)
      assert(empty.headers().firstValue("Transfer-Encoding").isEmpty)

      val head = client.send(
        HttpRequest
          .newBuilder(baseUri.resolve("non-empty.html"))
          .method("HEAD", HttpRequest.BodyPublishers.noBody())
          .build(),
        HttpResponse.BodyHandlers.ofByteArray(),
      )
      assertEquals(head.statusCode(), 200)
      assertEquals(
        head.headers().firstValue("Content-Length").orElse(null),
        nonEmptyBody
          .getBytes(java.nio.charset.StandardCharsets.UTF_8)
          .length
          .toString,
      )
      assertEquals(head.body().toSeq, Seq.empty)
      assert(head.headers().firstValue("Transfer-Encoding").isEmpty)

      val rejected = client.send(
        HttpRequest
          .newBuilder(baseUri)
          .POST(HttpRequest.BodyPublishers.noBody())
          .build(),
        HttpResponse.BodyHandlers.ofByteArray(),
      )
      assertEquals(rejected.statusCode(), 405)
      assertEquals(
        rejected.headers().firstValue("Allow").orElse(null),
        "GET, HEAD",
      )
    finally
      server.stop(0)
      GeneratedDirectory.delete(root)

  test("uses explicit UTF-8 content types"):
    assertEquals(
      StaticSiteHandler.contentType(java.nio.file.Path.of("index.html")),
      "text/html; charset=utf-8",
    )
    assertEquals(
      StaticSiteHandler.contentType(java.nio.file.Path.of("site.css")),
      "text/css; charset=utf-8",
    )

  test("releases the publication read lock before response streaming"):
    val root = Files.createTempDirectory("doctrellis-handler-lock")
    Files.writeString(root.resolve("index.html"), "streamed response")
    val lock     = PreviewPublicationLock()
    val started  = CountDownLatch(1)
    val release  = CountDownLatch(1)
    val acquired = CountDownLatch(1)
    val exchange = BlockingExchange(started, release)
    val pool     = Executors.newFixedThreadPool(2)
    try
      val response = pool.submit(
        new Callable[Unit]:
          override def call(): Unit =
            StaticSiteHandler(root, lock).handle(exchange),
      )
      assert(started.await(2L, TimeUnit.SECONDS))
      val publication = pool.submit(
        new Callable[Unit]:
          override def call(): Unit =
            lock.write:
              Files.writeString(root.resolve("index.html"), "replacement")
              acquired.countDown(),
      )
      assert(
        acquired.await(2L, TimeUnit.SECONDS),
        "publication remained blocked by response streaming",
      )
      publication.get(2L, TimeUnit.SECONDS)
      assert(!response.isDone, "the simulated client should still be stalled")
      release.countDown()
      response.get(2L, TimeUnit.SECONDS)
      assertEquals(
        String(exchange.responseBytes, java.nio.charset.StandardCharsets.UTF_8),
        "streamed response",
      )
      assertEquals(Files.readString(root.resolve("index.html")), "replacement")
    finally
      release.countDown()
      pool.shutdownNow()
      GeneratedDirectory.delete(root)

  private final class BlockingExchange(
      started: CountDownLatch,
      release: CountDownLatch,
      requestUri: URI = URI.create("/"),
      requestMethod: String = "GET",
      failOnResponseBody: Boolean = false,
  ) extends HttpExchange:
    private val requestHeaders  = Headers()
    private val responseHeaders = Headers()
    private val responseBody    = BlockingOutput(started, release)
    private var responseCode    = -1
    var sentResponseLength      = Long.MinValue
    var responseBodyRequested   = false
    var exchangeClosed          = false

    def responseBytes: Array[Byte] = responseBody.bytes

    override def getRequestHeaders(): Headers  = requestHeaders
    override def getResponseHeaders(): Headers = responseHeaders
    override def getRequestURI(): URI          = requestUri
    override def getRequestMethod(): String    = requestMethod
    override def close(): Unit                 = exchangeClosed = true
    override def getRequestBody(): InputStream =
      ByteArrayInputStream(Array.emptyByteArray)
    override def getResponseBody(): OutputStream =
      responseBodyRequested = true
      if failOnResponseBody then
        throw AssertionError("HEAD response body must not be opened")
      responseBody
    override def sendResponseHeaders(code: Int, length: Long): Unit =
      responseCode = code
      sentResponseLength = length
    override def getResponseCode(): Int = responseCode
    override def getProtocol(): String  = "HTTP/1.1"
    override def setStreams(input: InputStream, output: OutputStream): Unit =
      val _ = input
      val _ = output
    override def getHttpContext(): HttpContext         = unsupported()
    override def getRemoteAddress(): InetSocketAddress = unsupported()
    override def getLocalAddress(): InetSocketAddress  = unsupported()
    override def getAttribute(name: String): Object    = unsupported(name)
    override def setAttribute(name: String, value: Object): Unit =
      unsupported(name, value)
    override def getPrincipal(): HttpPrincipal = unsupported()

    private def unsupported[A](values: Object*): A =
      throw UnsupportedOperationException(values.mkString(","))

  private final class BlockingOutput(
      started: CountDownLatch,
      release: CountDownLatch,
  ) extends OutputStream:
    private val received = ByteArrayOutputStream()

    def bytes: Array[Byte] = received.toByteArray

    override def write(value: Int): Unit =
      block()
      received.write(value)

    override def write(bytes: Array[Byte], offset: Int, length: Int): Unit =
      block()
      received.write(bytes, offset, length)

    private def block(): Unit =
      started.countDown()
      try release.await()
      catch
        case error: InterruptedException =>
          Thread.currentThread().interrupt()
          throw java.io.IOException(
            "test response stream was interrupted",
            error,
          )
