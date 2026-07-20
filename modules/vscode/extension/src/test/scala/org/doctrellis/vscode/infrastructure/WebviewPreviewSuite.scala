package org.doctrellis.vscode.infrastructure

import scala.collection.mutable
import scala.concurrent.duration.*
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import cats.effect.{Deferred, IO}
import cats.syntax.all.*

import munit.CatsEffectSuite

import org.doctrellis.vscode.domain.*

class WebviewPreviewSuite extends CatsEffectSuite:
  private val first =
    Workspace("first", "First docs", "/first", "file:///first")
  private val second =
    Workspace("second", "Second docs", "/second", "file:///second")

  private def at[A](values: scala.collection.Seq[A], index: Int): A =
    values
      .lift(index)
      .getOrElse:
        fail(s"expected element $index in $values")

  test("reuses one panel per workspace and updates a restarted URL"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
          )
          _ <- registry.show(
            first,
            PreviewViewState.Ready(url(5252), supportsLiveRefresh = false),
          )
          _ <- registry.show(
            second,
            PreviewViewState.Ready(url(6262), supportsLiveRefresh = false),
          )
        yield
          assertEquals(backend.created.size, 2)
          assertEquals(at(backend.created, 0).reveals, 2)
          assert(at(backend.created, 0).html.contains("forwarded.example:5252"))
          assert(at(backend.created, 1).html.contains("forwarded.example:6262"))

  test("refreshes an existing panel without revealing or recreating it"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
          )
          panel = at(backend.created, 0)
          _ <- registry.update(
            first,
            PreviewViewState.Refreshed(url(4242), revision(2)),
          )
          _ = assertEquals(panel.reveals, 1)
          _ = assert(
            panel.html.contains("data-doctrellis-revision=\"2\""),
          )
          _ <- panel.disposeFromUser
          resolvedBefore = backend.resolved.size
          _ <- registry.update(
            first,
            PreviewViewState.Refreshed(url(4242), revision(3)),
          )
        yield
          assertEquals(backend.created.size, 1)
          assertEquals(backend.resolved.size, resolvedBefore)

  test("creates a new panel after the existing panel is disposed"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(first, PreviewViewState.Starting)
          _ <- at(backend.created, 0).disposeFromUser
          _ <- registry.show(first, PreviewViewState.Stopped)
        yield
          assertEquals(backend.created.size, 2)
          assert(at(backend.created, 1).html.contains("Preview stopped"))

  test("recreates a synchronously disposed panel before async removal"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(first, PreviewViewState.Starting)
          old = at(backend.created, 0)
          _ <- old.markDisposed
          _ <- registry.show(first, PreviewViewState.Stopped)
          _ <- old.notifyDisposed
          _ <- registry.show(first, PreviewViewState.Starting)
        yield
          assertEquals(backend.created.size, 2)
          assertEquals(at(backend.created, 1).reveals, 2)

  test("disposes every registered panel when the registry is released"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use(_.show(first, PreviewViewState.Starting))
      .map: _ =>
        assertEquals(backend.created.size, 1)
        assert(at(backend.created, 0).disposed)

  test("uses the externally resolved URI in the iframe"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        registry
          .show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
          )
          .map: _ =>
            assertEquals(backend.resolved.toList, List("http://127.0.0.1:4242"))
            assert(
              at(backend.created, 0).html.contains(
                "src=\"https://forwarded.example:4242/preview?remote=1&amp;view=full\"",
              ),
            )

  test("does not probe or enable the bridge for a fixed-URL ready state"):
    val bridgeResolutions = mutable.ListBuffer.empty[PreviewUrl]
    val backend           = new FakeBackend:
      override def resolveBridge(url: PreviewUrl): IO[Option[String]] =
        IO.delay:
          bridgeResolutions += url
          Some(
            "https://forwarded.example:4242/.doctrellis/webview-bridge",
          )
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        registry
          .show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
          )
          .map: _ =>
            val panel = at(backend.created, 0)
            assertEquals(bridgeResolutions.toList, Nil)
            assert(panel.html.contains("sandbox=\"\""))
            assert(!panel.html.contains("allow-scripts allow-same-origin"))

  test("refreshes through the bridge without replacing the current frame"):
    val bridgeResolutions = mutable.ListBuffer.empty[PreviewUrl]
    val backend           = new FakeBackend:
      override def resolveBridge(url: PreviewUrl): IO[Option[String]] =
        IO.delay:
          bridgeResolutions += url
          Some(
            "https://forwarded.example:4242/.doctrellis/webview-bridge",
          )
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = true),
          )
          panel       = at(backend.created, 0)
          initialHtml = panel.html
          _ <- registry.update(
            first,
            PreviewViewState.Refreshed(url(4242), revision(2)),
          )
        yield
          assertEquals(panel.html, initialHtml)
          assertEquals(panel.refreshes.toList, List(revision(2)))
          assertEquals(bridgeResolutions.toList, List(url(4242)))
          assertEquals(panel.channelTokens.size, 1)
          assert(panel.html.contains(at(panel.channelTokens, 0)))
          assertEquals(panel.reveals, 1)
          assert(
            panel.html.contains(
              "src=\"https://forwarded.example:4242/.doctrellis/webview-bridge\"",
            ),
          )
          assert(
            panel.html.contains(
              "data-doctrellis-preview-url=\"https://forwarded.example:4242/preview?remote=1&amp;view=full\"",
            ),
          )
          assert(
            panel.html.contains(
              "sandbox=\"allow-scripts allow-same-origin\"",
            ),
          )
          assert(
            panel.html.contains(
              "data-doctrellis-bridge-origin=\"https://forwarded.example:4242\"",
            ),
          )

  test("falls back to replacing HTML without a wrapper acknowledgement"):
    val backend = new FakeBackend:
      override def resolveBridge(url: PreviewUrl): IO[Option[String]] =
        IO.pure(url)
          .as(
            Some(
              "https://forwarded.example:4242/.doctrellis/webview-bridge",
            ),
          )

      override def create(
          workspace: Workspace,
          onDispose: IO[Unit],
      ): IO[PreviewPanel] =
        IO.pure(workspace)
          .flatMap: _ =>
            IO.delay:
              val panel = FakePanel(onDispose)
              panel.refreshAcknowledged = false
              created += panel
              panel
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = true),
          )
          panel       = at(backend.created, 0)
          initialHtml = panel.html
          _ <- registry.update(
            first,
            PreviewViewState.Refreshed(url(4242), revision(2)),
          )
        yield
          assertNotEquals(panel.html, initialHtml)
          assert(panel.html.contains("data-doctrellis-revision=\"2\""))
          assertEquals(panel.refreshes.toList, List(revision(2)))

  test(
    "does not hold the registry mutex while awaiting refresh acknowledgement",
  ):
    for
      refreshEntered <- Deferred[IO, Unit]
      releaseRefresh <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolveBridge(url: PreviewUrl): IO[Option[String]] =
          IO.pure(
            Some(
              "https://forwarded.example:4242/.doctrellis/webview-bridge",
            ),
          )

        override def create(
            workspace: Workspace,
            onDispose: IO[Unit],
        ): IO[PreviewPanel] =
          IO.pure(workspace)
            .flatMap: _ =>
              IO.delay:
                val panel = FakePanel(
                  onDispose,
                  refreshBehavior = Some: siteRevision =>
                    if siteRevision == revision(2) then
                      refreshEntered.complete(()).void *>
                        releaseRefresh.get.as(false)
                    else IO.pure(true),
                )
                created += panel
                panel
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            _ <- registry.show(
              first,
              PreviewViewState.Ready(url(4242), supportsLiveRefresh = true),
            )
            panel       = at(backend.created, 0)
            initialHtml = panel.html
            older <- registry
              .show(
                first,
                PreviewViewState.Refreshed(url(4242), revision(2)),
              )
              .start
            _     <- refreshEntered.get
            _     <- IO(assertEquals(panel.reveals, 2))
            newer <- registry
              .update(
                first,
                PreviewViewState.Refreshed(url(4242), revision(3)),
              )
              .timeout(1.second)
              .attempt
            _ <- releaseRefresh.complete(()).void
            _ <- older.joinWithNever
            _ <- IO.fromEither(newer)
          yield
            assertEquals(panel.html, initialHtml)
            assertEquals(
              panel.refreshes.toList,
              List(revision(2), revision(3)),
            )
    yield ()

  test("replaces the frame when a refresh state changes its resolved URLs"):
    val bridgeResolutions = mutable.ListBuffer.empty[PreviewUrl]
    val backend           = new FakeBackend:
      override def resolveBridge(url: PreviewUrl): IO[Option[String]] =
        IO.delay:
          bridgeResolutions += url
          val port = url.value.stripPrefix("http://127.0.0.1:")
          Some(
            s"https://forwarded.example:$port/.doctrellis/webview-bridge",
          )
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = true),
          )
          panel       = at(backend.created, 0)
          initialHtml = panel.html
          _ <- registry.update(
            first,
            PreviewViewState.Refreshed(url(5252), revision(2)),
          )
        yield
          assertNotEquals(panel.html, initialHtml)
          assert(panel.html.contains("forwarded.example:5252"))
          assert(panel.html.contains("data-doctrellis-revision=\"2\""))
          assertEquals(panel.refreshes.toList, Nil)
          assertEquals(
            bridgeResolutions.toList,
            List(url(4242), url(5252)),
          )

  test("invalidates the bridge cache for a new session without a panel"):
    val bridgeResolutions = mutable.ListBuffer.empty[PreviewUrl]
    val backend           = new FakeBackend:
      override def resolveBridge(url: PreviewUrl): IO[Option[String]] =
        IO.delay:
          bridgeResolutions += url
          Some(
            "https://forwarded.example:4242/.doctrellis/webview-bridge",
          )
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = true),
          )
          _ <- at(backend.created, 0).disposeFromUser
          _ <- registry.update(first, PreviewViewState.Starting)
          _ <- registry.show(
            first,
            PreviewViewState.Ready(url(4242), supportsLiveRefresh = true),
          )
        yield assertEquals(
          bridgeResolutions.toList,
          List(url(4242), url(4242)),
        )

  test("builds the bridge URL from the preview origin"):
    val preview = right(
      PreviewUrl.parse("https://docs.example:7443/base/?remote=1#section"),
    )
    assertEquals(
      PreviewWebviewBridge.url(preview).map(_.value),
      Right("https://docs.example:7443/.doctrellis/webview-bridge"),
    )

  test("accepts only an exact bridge response marker on status 200"):
    assert(NodeHttpBridgeProbe.accepts(200, Some("1")))
    assert(!NodeHttpBridgeProbe.accepts(200, None))
    assert(!NodeHttpBridgeProbe.accepts(200, Some("true")))
    assert(!NodeHttpBridgeProbe.accepts(204, Some("1")))
    assert(!NodeHttpBridgeProbe.accepts(302, Some("1")))

  test("accepts only a complete wrapper refresh acknowledgement"):
    val valid = js.Dynamic.literal(
      protocol = 1,
      `type` = "doctrellis.preview.refresh-accepted",
      refreshId = "attempt-1",
      revision = "7",
      channelToken = "secret",
    )
    assertEquals(
      PreviewWebviewMessage.refreshAcknowledgement(valid),
      Some(RefreshAcknowledgement("attempt-1", "7", "secret")),
    )
    assertEquals(
      PreviewWebviewMessage.refreshAcknowledgement(
        js.Dynamic.literal(
          protocol = 1,
          `type` = "doctrellis.preview.refresh-accepted",
          revision = "7",
          channelToken = "secret",
        ),
      ),
      None,
    )
    assertEquals(
      PreviewWebviewMessage.refreshAcknowledgement(
        js.Dynamic.literal(
          protocol = 1,
          `type` = "doctrellis.preview.refresh-accepted",
          refreshId = "attempt-1",
          revision = "7",
        ),
      ),
      None,
    )
    assertEquals(
      PreviewWebviewMessage.refreshAcknowledgement(
        js.Dynamic.literal(
          protocol = 2,
          `type` = "doctrellis.preview.refresh-accepted",
          refreshId = "attempt-1",
          revision = "7",
          channelToken = "secret",
        ),
      ),
      None,
    )

  test("requires an acknowledgement after Webview transport delivery"):
    val expected = RefreshAcknowledgement("attempt-1", "7", "secret")
    RefreshAcknowledgements
      .create(20.millis)
      .flatMap: acknowledgements =>
        for
          missing  <- acknowledgements.await(expected)(IO.pure(true))
          rejected <- acknowledgements.await(expected)(IO.pure(false))
          sent     <- Deferred[IO, Unit]
          accepted <- acknowledgements
            .await(expected)(sent.complete(()).void.as(true))
            .start
          _       <- sent.get
          _       <- acknowledgements.accept(expected)
          handled <- accepted.joinWithNever
        yield
          assert(!missing)
          assert(!rejected)
          assert(handled)

  test("tracks overlapping acknowledgements independently"):
    val firstAck  = RefreshAcknowledgement("attempt-1", "7", "secret")
    val secondAck = RefreshAcknowledgement("attempt-2", "7", "secret")
    RefreshAcknowledgements
      .create(1.second)
      .flatMap: acknowledgements =>
        for
          firstSent  <- Deferred[IO, Unit]
          secondSent <- Deferred[IO, Unit]
          firstWait  <- acknowledgements
            .await(firstAck)(firstSent.complete(()).void.as(true))
            .start
          _          <- firstSent.get
          secondWait <- acknowledgements
            .await(secondAck)(secondSent.complete(()).void.as(true))
            .start
          _             <- secondSent.get
          _             <- acknowledgements.accept(secondAck)
          _             <- acknowledgements.accept(firstAck)
          firstHandled  <- firstWait.joinWithNever
          secondHandled <- secondWait.joinWithNever
        yield
          assert(firstHandled)
          assert(secondHandled)

  test("escapes dynamic content and permits only the trusted bridge script"):
    val hostile = first.copy(name = "<docs> & \"team\" '")
    val ready   = PreviewWebviewHtml.render(
      hostile,
      PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
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
    assert(ready.contains(s"script-src ${PreviewWebviewHtml.ScriptHash}"))
    assert(ready.contains(s"<script>${PreviewWebviewHtml.Script}</script>"))
    assert(ready.indexOf("<script>") < ready.indexOf("</body>"))
    assert(!ready.contains("<docs>"))
    assert(ready.contains("&lt;docs&gt; &amp; &quot;team&quot; &#39;"))
    assert(ready.contains("one=1&amp;two=2"))
    assertEquals(failed.sliding("<script>".length).count(_ == "<script>"), 1)
    assert(
      failed.contains("bad &lt;script&gt;alert(&#39;x&#39;)&lt;/script&gt;"),
    )

  test("uses inert revision markup to make same-URL refreshes distinct"):
    val external = Some(
      resolved("https://forwarded.example:4242/preview"),
    )
    val second = PreviewWebviewHtml.render(
      first,
      PreviewViewState.Refreshed(url(4242), revision(2)),
      external,
    )
    val third = PreviewWebviewHtml.render(
      first,
      PreviewViewState.Refreshed(url(4242), revision(3)),
      external,
    )

    assertNotEquals(second, third)
    assert(second.contains("data-doctrellis-revision=\"2\""))
    assert(third.contains("data-doctrellis-revision=\"3\""))
    List(second, third).foreach: html =>
      assert(html.contains("default-src 'none'"))
      assert(html.contains("sandbox=\"\""))
      assert(html.contains(s"script-src ${PreviewWebviewHtml.ScriptHash}"))

  test("shows an unavailable state for a resolved URI with userinfo"):
    val actual = resolved(
      "https://user:token@remote_host.example:7443/preview",
    )
    val html = PreviewWebviewHtml.render(
      first,
      PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
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
      PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
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
      PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
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
              .show(
                first,
                PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
              )
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
              .show(
                first,
                PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
              )
              .start
            _      <- resolving.get
            failed <- registry
              .show(
                first,
                PreviewViewState.Ready(url(5252), supportsLiveRefresh = false),
              )
              .attempt
            _ <- IO.delay(assert(failed.isLeft))
            _ <- IO.delay:
              assert(at(backend.created, 0).html.contains("Preview failed"))
              assert(at(backend.created, 0).html.contains("resolution failed"))
            _ <- continue.complete(())
            _ <- stale.joinWithNever
          yield assert(
            at(backend.created, 0).html.contains("resolution failed"),
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
      .use(
        _.show(
          first,
          PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
        ).attempt,
      )
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
              .show(
                first,
                PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
              )
              .start
            _ <- resolving.get
            exit = ProcessExit(Some(1), None, None)
            _ <- registry.update(first, PreviewViewState.Exited(exit))
            _ <- continue.complete(())
            _ <- open.joinWithNever
          yield
            assertEquals(at(backend.created, 0).reveals, 2)
            assert(at(backend.created, 0).html.contains("Preview disconnected"))
    yield ()

  test("a refresh preserves a show reserved after synchronous disposal"):
    for
      resolving <- Deferred[IO, Unit]
      continue  <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolve(actual: PreviewUrl): IO[String] =
          if actual == WebviewPreviewSuite.this.url(4242) then
            resolving.complete(()).void *>
              continue.get.as("https://forwarded.example:4242/preview")
          else super.resolve(actual)
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            _ <- registry.show(first, PreviewViewState.Starting)
            old = at(backend.created, 0)
            _    <- old.markDisposed
            open <- registry
              .show(
                first,
                PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
              )
              .start
            _ <- resolving.get
            _ <- old.notifyDisposed
            _ <- registry.update(
              first,
              PreviewViewState.Refreshed(url(5252), revision(2)),
            )
            _           = assertEquals(backend.created.size, 2)
            replacement = at(backend.created, 1)
            _           = assertEquals(replacement.reveals, 1)
            _ = assert(replacement.html.contains("forwarded.example:5252"))
            _ = assert(
              replacement.html.contains("data-doctrellis-revision=\"2\""),
            )
            _ <- continue.complete(())
            _ <- open.joinWithNever
            _ <- registry.update(
              first,
              PreviewViewState.Refreshed(url(5252), revision(3)),
            )
          yield
            assertEquals(backend.created.size, 2)
            assertEquals(replacement.reveals, 1)
            assert(
              replacement.html.contains("data-doctrellis-revision=\"3\""),
            )
    yield ()

  test("a background refresh does not reopen a disposed panel"):
    val backend = FakeBackend()
    PreviewPanelRegistry
      .resource(backend)
      .use: registry =>
        for
          _ <- registry.show(first, PreviewViewState.Starting)
          old = at(backend.created, 0)
          _ <- old.markDisposed
          resolvedBefore = backend.resolved.size
          _ <- registry.update(
            first,
            PreviewViewState.Refreshed(url(4242), revision(2)),
          )
          _ <- old.notifyDisposed
          _ <- registry.update(
            first,
            PreviewViewState.Refreshed(url(4242), revision(3)),
          )
        yield
          assertEquals(backend.created.size, 1)
          assertEquals(backend.resolved.size, resolvedBefore)
          assertEquals(old.reveals, 1)

  test("a canceled refresh creation preserves another pending show"):
    for
      resolving         <- Deferred[IO, Unit]
      continue          <- Deferred[IO, Unit]
      replacementReveal <- Deferred[IO, Unit]
      backend = new FakeBackend:
        override def resolve(actual: PreviewUrl): IO[String] =
          if actual == WebviewPreviewSuite.this.url(4242) then
            resolving.complete(()).void *>
              continue.get.as("https://forwarded.example:4242/preview")
          else super.resolve(actual)

        override def create(
            workspace: Workspace,
            onDispose: IO[Unit],
        ): IO[PreviewPanel] =
          IO.delay:
            val beforeReveal =
              if created.size == 1 then
                replacementReveal.complete(()).void *> IO.never
              else IO.unit
            val panel = FakePanel(onDispose, beforeReveal = beforeReveal)
            created += panel
            panel
      _ <- PreviewPanelRegistry
        .resource(backend)
        .use: registry =>
          for
            _ <- registry.show(first, PreviewViewState.Starting)
            old = at(backend.created, 0)
            _    <- old.markDisposed
            open <- registry
              .show(
                first,
                PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
              )
              .start
            _       <- resolving.get
            _       <- old.notifyDisposed
            refresh <- registry
              .update(
                first,
                PreviewViewState.Refreshed(url(5252), revision(2)),
              )
              .start
            _ <- replacementReveal.get
            _ <- refresh.cancel
            _ = assert(at(backend.created, 1).disposed)
            _ <- continue.complete(())
            _ <- open.joinWithNever
          yield
            assertEquals(backend.created.size, 3)
            val replacement = at(backend.created, 2)
            assertEquals(replacement.reveals, 1)
            assert(replacement.html.contains("forwarded.example:4242"))
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
            old = at(backend.created, 0)
            open <- registry
              .show(
                first,
                PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
              )
              .start
            _ <- resolving.get
            _ <- old.markDisposed
            _ <- continue.complete(())
            _ <- open.joinWithNever
            _ <- old.notifyDisposed
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
              .show(
                first,
                PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
              )
              .start
            _ <- resolving.get
            _ <- registry.show(first, PreviewViewState.Starting)
            _ <- at(backend.created, 0).disposeFromUser
            _ <- registry.show(first, PreviewViewState.Stopped)
            _ <- continue.complete(())
            _ <- stale.joinWithNever
          yield
            assertEquals(backend.created.size, 2)
            assert(at(backend.created, 1).html.contains("Preview stopped"))
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
            .show(
              first,
              PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
            )
            .attempt
          _ <- registry.update(first, PreviewViewState.Stopped)
          _ <- registry.show(first, PreviewViewState.Stopped)
        yield
          assert(failed.isLeft)
          assertEquals(backend.created.size, 1)
          assert(at(backend.created, 0).html.contains("Preview stopped"))

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
          assert(at(created, 0).disposed)
          assert(at(created, 1).html.contains("Preview stopped"))

  test("attempts every panel disposal before propagating a cleanup failure"):
    val created = mutable.ListBuffer.empty[FakePanel]
    val backend = new PreviewPanelBackend:
      def resolve(url: PreviewUrl): IO[String] = IO.pure(url.value)

      def create(
          workspace: Workspace,
          onDispose: IO[Unit],
      ): IO[PreviewPanel] =
        IO.delay:
          val panel = FakePanel(onDispose, failOnDispose = true)
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
              assertEquals(
                result.swap.toOption.toVector.flatMap(_.getSuppressed).size,
                1,
              )
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
              .show(
                first,
                PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
              )
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
            def isDisposed: Boolean = false

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
        .show(
          first,
          PreviewViewState.Ready(url(4242), supportsLiveRefresh = false),
        )
        .start
      _ <- resolving.get
      _ <- release
      _ <- continue.complete(())
      _ <- display.joinWithNever
      _ <- registry.show(first, PreviewViewState.Stopped)
    yield
      assertEquals(backend.created.size, 1)
      assert(at(backend.created, 0).disposed)

  test("retains hidden context and enables only the trusted wrapper script"):
    val options = PreviewWebviewOptions.value.asInstanceOf[js.Dynamic]
    assertEquals(options.enableScripts.asInstanceOf[Boolean], true)
    assertEquals(options.enableCommandUris.asInstanceOf[Boolean], false)
    assertEquals(
      options.localResourceRoots.asInstanceOf[js.Array[js.Any]].length,
      0,
    )
    assertEquals(
      options.retainContextWhenHidden.asInstanceOf[Boolean],
      true,
    )

  test("keeps the CSP style hash synchronized with the rendered styles"):
    val digest = TestNodeCrypto("sha256")
      .update(PreviewWebviewHtml.Styles)
      .digest("base64")
    assertEquals(PreviewWebviewHtml.StylesHash, s"'sha256-$digest'")

  test("keeps the CSP script hash synchronized with the trusted script"):
    val digest = TestNodeCrypto("sha256")
      .update(PreviewWebviewHtml.Script)
      .digest("base64")
    assertEquals(PreviewWebviewHtml.ScriptHash, s"'sha256-$digest'")
    assert(PreviewWebviewHtml.Script.contains("doctrellis.preview.refresh"))
    assert(PreviewWebviewHtml.Script.contains("event.origin !== bridgeOrigin"))
    assert(PreviewWebviewHtml.Script.contains("event.source !== parent"))
    assert(
      !PreviewWebviewHtml.Script.contains("event.origin !== location.origin"),
    )
    assert(PreviewWebviewHtml.Script.contains("acquireVsCodeApi()"))
    assert(
      PreviewWebviewHtml.Script.indexOf("armFallback();") <
        PreviewWebviewHtml.Script.indexOf("vscode = acquireVsCodeApi()"),
    )
    assert(
      PreviewWebviewHtml.Script.contains(
        "document.addEventListener(\"visibilitychange\"",
      ),
    )
    assert(
      PreviewWebviewHtml.Script.contains(
        "document.visibilityState === \"visible\"",
      ),
    )
    assert(
      PreviewWebviewHtml.Script.contains(
        "doctrellis.preview.refresh-accepted",
      ),
    )
    assert(PreviewWebviewHtml.Script.contains("message.refreshId"))
    assert(PreviewWebviewHtml.Script.contains("message.channelToken"))
    assert(!PreviewWebviewHtml.Script.contains("\"*\""))
    assert(!PreviewWebviewHtml.Script.contains("eval("))

  private def url(port: Int): PreviewUrl =
    right(PreviewUrl.parse(s"http://127.0.0.1:$port"))

  private def resolved(value: String): ResolvedPreviewUri =
    ResolvedPreviewUri.parse(value).fold(fail(_), identity)

  private def revision(value: Long): SiteRevision =
    SiteRevision.parse(value).fold(problem => fail(problem.message), identity)

  private def right[A](value: Either[ConfigProblem, A]): A =
    value.fold(problem => fail(problem.message), identity)

private final class FakePanel(
    val onDispose: IO[Unit],
    failOnDispose: Boolean = false,
    failOnSetHtml: Boolean = false,
    beforeReveal: IO[Unit] = IO.unit,
    refreshBehavior: Option[SiteRevision => IO[Boolean]] = None,
) extends PreviewPanel:
  var html: String                                = ""
  var reveals: Int                                = 0
  var disposed: Boolean                           = false
  var refreshAcknowledged: Boolean                = true
  val refreshes: mutable.ListBuffer[SiteRevision] = mutable.ListBuffer.empty
  val channelTokens: mutable.ListBuffer[String]   = mutable.ListBuffer.empty

  def isDisposed: Boolean = disposed

  def setHtml(value: String): IO[Unit] =
    IO.delay:
      if disposed then throw PreviewPanelDisposed
      else if failOnSetHtml then throw IllegalStateException("write failed")
      else html = value

  override def refresh(
      revision: SiteRevision,
      channelToken: String,
  ): IO[Boolean] =
    IO.delay:
      if disposed then throw PreviewPanelDisposed
      else
        refreshes += revision
        channelTokens += channelToken
    *> refreshBehavior
      .fold(IO.pure(refreshAcknowledged))(_(revision))

  def reveal: IO[Unit] =
    beforeReveal *>
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
      val prefix = "http://127.0.0.1:"
      val port   =
        if url.value.startsWith(prefix) then url.value.drop(prefix.length)
        else
          throw IllegalArgumentException(s"unexpected test URL: ${url.value}")
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
