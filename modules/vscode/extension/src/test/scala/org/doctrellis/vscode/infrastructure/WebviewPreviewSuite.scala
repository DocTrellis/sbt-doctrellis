package org.doctrellis.vscode.infrastructure

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.doctrellis.vscode.domain.*

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

class WebviewPreviewSuite extends CatsEffectSuite:
  private val first =
    Workspace("first", "First docs", "/first", "file:///first")
  private val second =
    Workspace("second", "Second docs", "/second", "file:///second")

  test("reuses one panel per workspace and updates a restarted URL"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(first, PreviewViewState.Ready(url(4242)))
          _ <- registry.show(first, PreviewViewState.Ready(url(5252)))
          _ <- registry.show(second, PreviewViewState.Ready(url(6262)))
        yield
          assertEquals(backend.created.size, 2)
          assertEquals(backend.created.head.reveals, 2)
          assert(backend.created.head.html.contains("forwarded.example:5252"))
          assert(backend.created(1).html.contains("forwarded.example:6262"))

  test("creates a new panel after the existing panel is disposed"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(first, PreviewViewState.Starting)
          _ <- backend.created.head.disposeFromUser
          _ <- registry.show(first, PreviewViewState.Stopped)
        yield
          assertEquals(backend.created.size, 2)
          assert(backend.created(1).html.contains("Preview stopped"))

  test("recreates a synchronously disposed panel before async removal"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(first, PreviewViewState.Starting)
          old = backend.created.head
          _ <- old.markDisposed
          _ <- registry.show(first, PreviewViewState.Stopped)
          _ <- old.notifyDisposed
          _ <- registry.show(first, PreviewViewState.Starting)
        yield
          assertEquals(backend.created.size, 2)
          assertEquals(backend.created(1).reveals, 2)

  test("disposes every registered panel when the registry is released"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use(_.show(first, PreviewViewState.Starting))
      .map: _ =>
        assertEquals(backend.created.size, 1)
        assert(backend.created.head.disposed)

  test("uses the externally resolved URI in the iframe"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        registry
          .show(first, PreviewViewState.Ready(url(4242)))
          .map: _ =>
            assertEquals(backend.resolved.toList, List("http://127.0.0.1:4242"))
            assert(
              backend.created.head.html.contains(
                "src=\"https://forwarded.example:4242/preview?remote=1&amp;view=full\"",
              ),
            )

  test("escapes dynamic content and emits a restrictive script-free CSP"):
    val hostile = first.copy(name = "<docs> & \"team\" '")
    val ready   = PreviewWebviewHtml.render(
      hostile,
      PreviewViewState.Ready(url(4242)),
      Some(
        resolved("https://forwarded.example:4242/preview?one=1&two=2"),
      ),
    )
    val failed = PreviewWebviewHtml.render(
      hostile,
      PreviewViewState.Failed("bad <script>alert('x')</script>"),
      None,
    )

    assert(ready.contains("default-src 'none'"))
    assert(ready.contains("frame-src https://forwarded.example:4242;"))
    assert(
      ready.contains(
        "style-src 'sha256-0Xd2UqyuR101toiMU/5s4npyG081HHWxTpXtf3fr0Ww='",
      ),
    )
    assert(!ready.contains("unsafe-inline"))
    assert(ready.contains("sandbox=\"\""))
    assert(!ready.contains("<script"))
    assert(!ready.contains("<docs>"))
    assert(ready.contains("&lt;docs&gt; &amp; &quot;team&quot; &#39;"))
    assert(ready.contains("one=1&amp;two=2"))
    assert(!failed.contains("<script>"))
    assert(
      failed.contains("bad &lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt;"),
    )

  test("shows an unavailable state for a resolved URI with userinfo"):
    val actual = resolved(
      "https://user:token@remote_host.example:7443/preview",
    )
    val html = PreviewWebviewHtml.render(
      first,
      PreviewViewState.Ready(url(4242)),
      Some(actual),
    )
    assertEquals(actual.cspOrigin, None)
    assert(html.contains("Preview unavailable"))
    assert(html.contains("Open in External Browser"))
    assert(!html.contains("<iframe"))

  test("shows an unavailable state for an IPv6 literal CSP origin"):
    val external = resolved("http://[::1]:4242/preview")
    val html     = PreviewWebviewHtml.render(
      first,
      PreviewViewState.Ready(url(4242)),
      Some(external),
    )
    assertEquals(external.cspOrigin, None)
    assert(html.contains("Preview unavailable"))
    assert(html.contains("Open in External Browser"))
    assert(!html.contains("<iframe"))
    assertEquals(
      resolved("http://192.168.1.10:4242/").cspOrigin,
      None,
    )

  test("embeds plain HTTP only for localhost origins"):
    val remote = resolved("http://host.docker.internal:4242/preview")
    val html   = PreviewWebviewHtml.render(
      first,
      PreviewViewState.Ready(url(4242)),
      Some(remote),
    )
    assertEquals(remote.cspOrigin, None)
    assert(html.contains("Preview unavailable"))
    assert(!html.contains("<iframe"))
    assertEquals(
      resolved("http://docs.localhost:4242/preview").cspOrigin,
      Some("http://docs.localhost:4242"),
    )
    assertEquals(
      resolved("https://host.docker.internal:4242/preview").cspOrigin,
      Some("https://host.docker.internal:4242"),
    )

  test("rejects unsafe or malformed resolved preview URIs"):
    val rejected = List(
      "javascript:alert(1)",
      "vscode://preview",
      "https://host:0/",
      "https://host:65536/",
      "https://bad host/",
      "https://host;frame-src *",
    )
    rejected.foreach(value => assert(ResolvedPreviewUri.parse(value).isLeft))

  test("passes only the externally resolved URI to the external opener"):
    val opened   = mutable.ListBuffer.empty[String]
    val external = ExternalPreview(
      _ => IO.pure("https://forwarded.example:7443/preview"),
      value => IO.delay(opened += value).as(true),
    )
    external
      .open(url(4242))
      .map: _ =>
        assertEquals(
          opened.toList,
          List("https://forwarded.example:7443/preview"),
        )

  test("does not let a slow ready URI overwrite a later stopped state"):
    for
      resolving <- Deferred[IO, Unit]
      continue  <- Deferred[IO, Unit]
      created   <- Deferred[IO, FakePanel]
      backend = new PreviewPanelBackend:
        def resolve(url: PreviewUrl): IO[String] =
          resolving.complete(()).void *>
            continue.get.as("https://forwarded.example:4242/preview")

        def create(
            workspace: Workspace,
            onDispose: IO[Unit],
        ): IO[PreviewPanel] =
          IO.delay(FakePanel(onDispose)).flatTap(created.complete).widen
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            ready <- registry
              .show(first, PreviewViewState.Ready(url(4242)))
              .start
            _       <- resolving.get
            stopped <- registry.update(first, PreviewViewState.Stopped).start
            _       <- stopped.joinWithNever
            panel   <- created.get
            _       <- IO.delay(assert(panel.html.contains("Preview stopped")))
            _       <- continue.complete(())
            _       <- ready.joinWithNever
          yield assert(panel.html.contains("Preview stopped"))
    yield ()

  test("a failed newest URI resolution replaces stale panel content"):
    for
      resolving <- Deferred[IO, Unit]
      continue  <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolve(url: PreviewUrl): IO[String] =
          if url == WebviewPreviewSuite.this.url(4242) then
            resolving.complete(()).void *>
              continue.get.as("https://forwarded.example:4242/preview")
          else IO.raiseError(IllegalStateException("resolution failed"))
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            _     <- registry.show(first, PreviewViewState.Starting)
            stale <- registry
              .show(first, PreviewViewState.Ready(url(4242)))
              .start
            _      <- resolving.get
            failed <- registry
              .show(first, PreviewViewState.Ready(url(5252)))
              .attempt
            _ <- IO.delay(assert(failed.isLeft))
            _ <- IO.delay:
              assert(backend.created.head.html.contains("Preview failed"))
              assert(backend.created.head.html.contains("resolution failed"))
            _ <- continue.complete(())
            _ <- stale.joinWithNever
          yield assert(
            backend.created.head.html.contains("resolution failed"),
          )
    yield ()

  test("preserves a URI resolution failure when its fallback cannot render"):
    val primary   = IllegalStateException("resolution failed")
    val secondary = IllegalStateException("panel creation failed")
    val backend   = new PreviewPanelBackend:
      def resolve(url: PreviewUrl): IO[String] = IO.raiseError(primary)

      def create(
          workspace: Workspace,
          onDispose: IO[Unit],
      ): IO[PreviewPanel] = IO.raiseError(secondary)

    PreviewPanelRegistry
      .resource(backend)
      .use(_.show(first, PreviewViewState.Ready(url(4242))).attempt)
      .map(assertEquals(_, Left(primary)))

  test("preserves reveal intent when a newer background state wins"):
    for
      resolving <- Deferred[IO, Unit]
      continue  <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolve(url: PreviewUrl): IO[String] =
          resolving.complete(()).void *>
            continue.get.as("https://forwarded.example:4242/preview")
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            _    <- registry.show(first, PreviewViewState.Starting)
            open <- registry
              .show(first, PreviewViewState.Ready(url(4242)))
              .start
            _ <- resolving.get
            exit = ProcessExit(Some(1), None, None)
            _ <- registry.update(first, PreviewViewState.Exited(exit))
            _ <- continue.complete(())
            _ <- open.joinWithNever
          yield
            assertEquals(backend.created.head.reveals, 2)
            assert(backend.created.head.html.contains("Preview disconnected"))
    yield ()

  test("a user disposal after a pending open wins over that open"):
    for
      resolving <- Deferred[IO, Unit]
      continue  <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolve(url: PreviewUrl): IO[String] =
          resolving.complete(()).void *>
            continue.get.as("https://forwarded.example:4242/preview")
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            _ <- registry.show(first, PreviewViewState.Starting)
            old = backend.created.head
            open <- registry
              .show(first, PreviewViewState.Ready(url(4242)))
              .start
            _ <- resolving.get
            _ <- old.disposeFromUser
            _ <- continue.complete(())
            _ <- open.joinWithNever
            _ = assertEquals(backend.created.size, 1)
            _ <- registry.show(first, PreviewViewState.Stopped)
          yield assertEquals(backend.created.size, 2)
    yield ()

  test("does not reuse a removed slot generation for an in-flight display"):
    for
      resolving <- Deferred[IO, Unit]
      continue  <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolve(url: PreviewUrl): IO[String] =
          resolving.complete(()).void *>
            continue.get.as("https://forwarded.example:4242/preview")
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            stale <- registry
              .show(first, PreviewViewState.Ready(url(4242)))
              .start
            _ <- resolving.get
            _ <- registry.show(first, PreviewViewState.Starting)
            _ <- backend.created.head.disposeFromUser
            _ <- registry.show(first, PreviewViewState.Stopped)
            _ <- continue.complete(())
            _ <- stale.joinWithNever
          yield
            assertEquals(backend.created.size, 2)
            assert(backend.created(1).html.contains("Preview stopped"))
    yield ()

  test("abandons a failed URI resolution before a later show"):
    val backend = new FakeBackend:
      override def resolve(url: PreviewUrl): IO[String] =
        IO.raiseError(IllegalStateException("resolution failed"))

    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          failed <- registry
            .show(first, PreviewViewState.Ready(url(4242)))
            .attempt
          _ <- registry.update(first, PreviewViewState.Stopped)
          _ <- registry.show(first, PreviewViewState.Stopped)
        yield
          assert(failed.isLeft)
          assertEquals(backend.created.size, 1)
          assert(backend.created.head.html.contains("Preview stopped"))

  test("disposes and removes a panel whose initial HTML write fails"):
    val created = mutable.ListBuffer.empty[FakePanel]
    val backend = new PreviewPanelBackend:
      def resolve(url: PreviewUrl): IO[String] = IO.pure(url.value)

      def create(
          workspace: Workspace,
          onDispose: IO[Unit],
      ): IO[PreviewPanel] =
        IO.delay:
          val panel = FakePanel(
            onDispose,
            failOnSetHtml = created.isEmpty,
          )
          created += panel
          panel

    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          failed <- registry.show(first, PreviewViewState.Starting).attempt
          _      <- registry.show(first, PreviewViewState.Stopped)
        yield
          assert(failed.isLeft)
          assertEquals(created.size, 2)
          assert(created.head.disposed)
          assert(created(1).html.contains("Preview stopped"))

  test("attempts every panel disposal before propagating a cleanup failure"):
    val created = mutable.ListBuffer.empty[FakePanel]
    val backend = new PreviewPanelBackend:
      def resolve(url: PreviewUrl): IO[String] = IO.pure(url.value)

      def create(
          workspace: Workspace,
          onDispose: IO[Unit],
      ): IO[PreviewPanel] =
        IO.delay:
          val panel = FakePanel(onDispose, failOnDispose = created.isEmpty)
          created += panel
          panel

    PreviewPanelRegistry
      .resource(backend)
      .allocated
      .flatMap:
        case (registry, release) =>
          registry.show(first, PreviewViewState.Starting) *>
            registry.show(second, PreviewViewState.Starting) *>
            release.attempt.map: result =>
              assert(result.isLeft)
              assertEquals(created.map(_.disposed).toList, List(true, true))

  test("cancellation abandons an empty display reservation"):
    for
      resolving <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolve(url: PreviewUrl): IO[String] =
          resolving.complete(()).void *> IO.never
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            display <- registry
              .show(first, PreviewViewState.Ready(url(4242)))
              .start
            _ <- resolving.get
            _ <- display.cancel
            _ <- registry.update(first, PreviewViewState.Stopped)
          yield assertEquals(backend.created.size, 0)
    yield ()

  test("cancellation disposes a panel acquired before registration"):
    for
      writeEntered <- Deferred[IO, Unit]
      disposed     <- Deferred[IO, Unit]
      backend = new PreviewPanelBackend:
        def resolve(url: PreviewUrl): IO[String] = IO.pure(url.value)

        def create(
            workspace: Workspace,
            onDispose: IO[Unit],
        ): IO[PreviewPanel] = IO.pure:
          new PreviewPanel:
            def setHtml(value: String): IO[Unit] =
              writeEntered.complete(()).void *> IO.never

            def reveal: IO[Unit] = IO.unit

            def dispose: IO[Unit] = disposed.complete(()).void
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            display <- registry.show(first, PreviewViewState.Starting).start
            _       <- writeEntered.get
            _       <- display.cancel
            _       <- disposed.get
          yield ()
    yield ()

  test("a display completing after registry release cannot recreate a panel"):
    for
      resolving <- Deferred[IO, Unit]
      continue  <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolve(url: PreviewUrl): IO[String] =
          resolving.complete(()).void *>
            continue.get.as("https://forwarded.example:4242/preview")
      allocated <- PreviewPanelRegistry.resource(backend).allocated
      (registry, release) = allocated
      _       <- registry.show(first, PreviewViewState.Starting)
      display <- registry
        .show(first, PreviewViewState.Ready(url(4242)))
        .start
      _ <- resolving.get
      _ <- release
      _ <- continue.complete(())
      _ <- display.joinWithNever
      _ <- registry.show(first, PreviewViewState.Stopped)
    yield
      assertEquals(backend.created.size, 1)
      assert(backend.created.head.disposed)

  test("disables scripts, command URIs, and local resource roots"):
    val options = PreviewWebviewOptions.value.asInstanceOf[js.Dynamic]
    assertEquals(options.enableScripts.asInstanceOf[Boolean], false)
    assertEquals(options.enableCommandUris.asInstanceOf[Boolean], false)
    assertEquals(
      options.localResourceRoots.asInstanceOf[js.Array[js.Any]].length,
      0,
    )

  test("keeps the CSP style hash synchronized with the rendered styles"):
    val digest = TestNodeCrypto("sha256")
      .update(PreviewWebviewHtml.Styles)
      .digest("base64")
    assertEquals(PreviewWebviewHtml.StylesHash, s"'sha256-$digest'")

  private def url(port: Int): PreviewUrl =
    right(PreviewUrl.parse(s"http://127.0.0.1:$port"))

  private def resolved(value: String): ResolvedPreviewUri =
    ResolvedPreviewUri.parse(value).fold(fail(_), identity)

  private def right[A](value: Either[ConfigProblem, A]): A =
    value.fold(problem => fail(problem.message), identity)

private final class FakePanel(
    val onDispose: IO[Unit],
    failOnDispose: Boolean = false,
    failOnSetHtml: Boolean = false,
) extends PreviewPanel:
  var html: String      = ""
  var reveals: Int      = 0
  var disposed: Boolean = false

  def setHtml(value: String): IO[Unit] =
    IO.delay:
      if disposed then throw PreviewPanelDisposed
      else if failOnSetHtml then throw IllegalStateException("write failed")
      else html = value

  def reveal: IO[Unit] =
    IO.delay:
      if disposed then throw PreviewPanelDisposed
      else reveals += 1

  def dispose: IO[Unit] =
    IO.delay:
      disposed = true
      if failOnDispose then throw IllegalStateException("dispose failed")

  def disposeFromUser: IO[Unit] =
    markDisposed *> notifyDisposed

  def markDisposed: IO[Unit] = IO.delay { disposed = true }

  def notifyDisposed: IO[Unit] = onDispose

private class FakeBackend extends PreviewPanelBackend:
  val created: mutable.ListBuffer[FakePanel] = mutable.ListBuffer.empty
  val resolved: mutable.ListBuffer[String]   = mutable.ListBuffer.empty

  def resolve(url: PreviewUrl): IO[String] =
    IO.delay:
      resolved += url.value
      val port = url.value.split(':').last
      s"https://forwarded.example:$port/preview?remote=1&view=full"

  def create(
      workspace: Workspace,
      onDispose: IO[Unit],
  ): IO[PreviewPanel] =
    IO.delay:
      val panel = FakePanel(onDispose)
      created += panel
      panel

private object FakeBackend:
  def apply(): FakeBackend = new FakeBackend

@js.native
private trait TestHash extends js.Object:
  def update(value: String): TestHash  = js.native
  def digest(encoding: String): String = js.native

@js.native
@JSImport("node:crypto", "createHash")
private object TestNodeCrypto extends js.Object:
  def apply(algorithm: String): TestHash = js.native
