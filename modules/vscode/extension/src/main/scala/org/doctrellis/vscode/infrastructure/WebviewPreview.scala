package org.doctrellis.vscode.infrastructure

import scala.concurrent.duration.*
import scala.scalajs.js

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.effect.std.{Dispatcher, Mutex}
import cats.syntax.all.*

import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

private[infrastructure] trait PreviewPanel:
  // Updated synchronously by the backend's disposal callback, before its IO
  // cleanup is dispatched, so a later explicit open can be ordered correctly.
  def isDisposed: Boolean
  def setHtml(value: String, channelToken: Option[String]): IO[Unit]
  // A backend without a message channel reports every refresh as unacknowledged.
  def refresh(
      revision: SiteRevision,
      channelToken: String,
  ): IO[Boolean] = IO.pure((revision, channelToken)).as(false)
  def reveal: IO[Unit]
  def dispose: IO[Unit]

private[infrastructure] case object PreviewPanelDisposed
    extends RuntimeException("The DocTrellis preview panel was disposed.")

private[infrastructure] trait PreviewPanelBackend:
  def resolve(url: PreviewUrl): IO[String]
  // A backend without bridge discovery always selects the direct iframe.
  def resolveBridge(url: PreviewUrl): IO[Option[String]] =
    IO.pure(url).as(None)
  def create(
      workspace: Workspace,
      onDispose: IO[Unit],
  ): IO[PreviewPanel]

private[infrastructure] final case class ResolvedPreviewUri private (
    value: String,
    cspOrigin: Option[String],
)

private[infrastructure] object ResolvedPreviewUri:
  private val HttpUri =
    """^(https?)://(?:([^\s/@?#\\]+)@)?(\[[0-9A-Fa-f:.]+\]|[A-Za-z0-9._~-]+)(?::([0-9]{1,5}))?([/?#][^\s]*)?$""".r
  private val Ipv4 = """[0-9]+(?:\.[0-9]+){3}""".r

  def parse(value: String): Either[String, ResolvedPreviewUri] =
    value match
      case HttpUri(scheme, userInfo, host, port, _) =>
        val portOption = Option(port)
        val validPort  = portOption.forall(
          _.toIntOption.exists(number => number > 0 && number <= 65535),
        )
        val normalizedHost = host.toLowerCase
        val localName      = normalizedHost == "localhost" ||
          normalizedHost.endsWith(".localhost")
        val trustworthy = scheme == "https" ||
          normalizedHost == "127.0.0.1" || localName
        Either.cond(
          validPort,
          ResolvedPreviewUri(
            value,
            Option.when(
              !host.startsWith("[") &&
                Option(userInfo).isEmpty &&
                trustworthy &&
                (!Ipv4.matches(host) || host == "127.0.0.1"),
            )(
              s"$scheme://$host${portOption.map(value => s":$value").getOrElse("")}",
            ),
          ),
          "the explicit port must be between 1 and 65535",
        )
      case _ => Left("the value must be an absolute HTTP(S) URI")

private[infrastructure] object PreviewWebviewBridge:
  // Keep this path synchronized with StaticSiteHandler.WebviewBridgePath in
  // the sbt plugin; each module has a contract test for the literal.
  val Path = "/.doctrellis/webview-bridge"

  def url(preview: PreviewUrl): Either[String, PreviewUrl] =
    for
      parsed <- ResolvedPreviewUri.parse(preview.value)
      origin <- parsed.cspOrigin.toRight(
        "the preview origin cannot host the Webview bridge",
      )
      bridge <- PreviewUrl.parse(s"$origin$Path").leftMap(_.message)
    yield bridge

private final case class RefreshTarget(
    previewUrl: String,
    bridgeUrl: String,
):
  def sameTarget(other: RefreshTarget): Boolean =
    previewUrl == other.previewUrl && bridgeUrl == other.bridgeUrl

private final case class RefreshChannel(target: RefreshTarget, token: String)

private final case class CachedBridge(
    previewUrl: PreviewUrl,
    resolved: Option[String],
)

private final case class BridgeSession(
    generation: Object,
    cached: Option[CachedBridge],
)

private final case class PanelInstallation(
    html: String,
    channel: Option[RefreshChannel],
)

private final case class RenderedPanel(
    target: Option[RefreshTarget],
    refresh: Option[SiteRevision],
    installation: IO[PanelInstallation],
)

private[infrastructure] final class PreviewPanelRegistry private (
    backend: PreviewPanelBackend,
    panels: Ref[IO, Map[String, PreviewPanelRegistry.Slot]],
    bridges: Ref[IO, Map[String, BridgeSession]],
    closed: Ref[IO, Boolean],
    mutex: Mutex[IO],
):
  import PreviewPanelRegistry.{Entry, Reservation, Revision, Slot}

  private final case class DeferredPanelRefresh(
      workspace: Workspace,
      reservation: Reservation,
      entry: Entry,
      rendered: RenderedPanel,
      revision: SiteRevision,
      channelToken: String,
  )

  def show(workspace: Workspace, state: PreviewViewState): IO[Unit] =
    display(workspace, state, open = true)

  def update(workspace: Workspace, state: PreviewViewState): IO[Unit] =
    display(workspace, state, open = false)

  private def display(
      workspace: Workspace,
      state: PreviewViewState,
      open: Boolean,
  ): IO[Unit] =
    resetBridgeSession(workspace.key, state) *>
      reserve(workspace.key, open).flatMap:
        case None              => IO.unit
        case Some(reservation) =>
          render(workspace, state).attempt
            .flatMap:
              case Right(rendered) => commit(workspace, reservation, rendered)
              case Left(error)     =>
                val fallback = RenderedPanel(
                  target = None,
                  refresh = None,
                  installation = IO.pure(
                    PanelInstallation(
                      PreviewWebviewHtml.render(
                        workspace,
                        PreviewViewState.Failed(message(error)),
                        None,
                      ),
                      channel = None,
                    ),
                  ),
                )
                commit(workspace, reservation, fallback).attempt.void *>
                  IO.raiseError(error)
            .onError(_ => abandon(workspace.key, reservation))
            .onCancel(abandon(workspace.key, reservation))

  private def reserve(
      key: String,
      open: Boolean,
  ): IO[Option[Reservation]] =
    mutex.lock.surround:
      closed.get.ifM(
        IO.pure(None),
        panels.modify: current =>
          current.get(key) match
            case Some(slot) if slot.panel.exists(_.panel.isDisposed) =>
              if open then
                val revision = new Revision
                current.updated(
                  key,
                  Slot(revision, Some(revision), panel = None),
                ) -> Some(Reservation(revision, open = true))
              else current.removed(key) -> None
            case Some(slot) =>
              val revision = new Revision
              val updated  = slot.copy(
                revision = revision,
                openIntent = Option.when(open)(revision).orElse(slot.openIntent),
              )
              current.updated(key, updated) ->
                Some(Reservation(revision, open))
            case None if open =>
              val revision = new Revision
              current.updated(
                key,
                Slot(revision, Some(revision), panel = None),
              ) ->
                Some(Reservation(revision, open = true))
            case None => current -> None,
      )

  private def commit(
      workspace: Workspace,
      reservation: Reservation,
      rendered: RenderedPanel,
  ): IO[Unit] =
    mutex.lock
      .surround(prepareCommit(workspace, reservation, rendered))
      .flatMap(_.traverse_(completeRefresh))

  private def prepareCommit(
      workspace: Workspace,
      reservation: Reservation,
      rendered: RenderedPanel,
  ): IO[Option[DeferredPanelRefresh]] =
    closed.get.ifM(
      IO.none,
      panels.get.flatMap:
        _.get(workspace.key) match
          case Some(slot) if slot.revision.eq(reservation.revision) =>
            slot.panel match
              case Some(entry) if entry.panel.isDisposed =>
                panels.update(_.removed(workspace.key)).as(None)
              case Some(entry) =>
                deferRefresh(
                  workspace,
                  reservation,
                  entry,
                  rendered,
                ).fold(
                  updateEntry(
                    workspace,
                    slot,
                    entry,
                    rendered,
                    replacePanel(entry, rendered),
                  ).as(None),
                )(refresh => revealThenDefer(slot, refresh))
              case None =>
                slot.openIntent
                  .fold(
                    panels.update(_.removed(workspace.key)),
                  )(intent => create(workspace, slot, rendered, intent))
                  .as(None)
          case Some(slot)
              if reservation.open && slot.openIntent.exists(
                _.eq(reservation.revision),
              ) =>
            slot.panel match
              case Some(entry) if entry.panel.isDisposed =>
                panels.update(_.removed(workspace.key)).as(None)
              case Some(entry) =>
                entry.panel.reveal
                  .flatMap(_ =>
                    consumeOpenIntent(
                      workspace.key,
                      reservation.revision,
                    ),
                  )
                  .handleErrorWith:
                    case PreviewPanelDisposed =>
                      if entry.panel.isDisposed then
                        panels.update(_.removed(workspace.key))
                      else
                        val empty = slot.copy(panel = None)
                        panels.update(
                          _.updated(workspace.key, empty),
                        ) *>
                          create(
                            workspace,
                            empty,
                            rendered,
                            reservation.revision,
                          )
                    case error => IO.raiseError(error)
                  .as(None)
              case None =>
                create(
                  workspace,
                  slot,
                  rendered,
                  reservation.revision,
                ).as(None)
          case _ => IO.none,
    )

  private def deferRefresh(
      workspace: Workspace,
      reservation: Reservation,
      entry: Entry,
      rendered: RenderedPanel,
  ): Option[DeferredPanelRefresh] =
    (rendered.refresh, entry.channel, rendered.target) match
      case (Some(revision), Some(installed), Some(next))
          if installed.target.sameTarget(next) =>
        Some:
          DeferredPanelRefresh(
            workspace,
            reservation,
            entry,
            rendered,
            revision,
            installed.token,
          )
      case _ => None

  private def revealThenDefer(
      slot: Slot,
      refresh: DeferredPanelRefresh,
  ): IO[Option[DeferredPanelRefresh]] =
    updateEntry(
      refresh.workspace,
      slot,
      refresh.entry,
      refresh.rendered,
      IO.unit,
    ) *>
      panels.get.map: current =>
        current.get(refresh.workspace.key) match
          case Some(actual)
              if actual.revision.eq(refresh.reservation.revision) &&
                actual.panel.exists(_.token.eq(refresh.entry.token)) =>
            Some(refresh)
          case _ => None

  private def completeRefresh(refresh: DeferredPanelRefresh): IO[Unit] =
    refresh.entry.panel
      .refresh(refresh.revision, refresh.channelToken)
      .attempt
      .flatMap: outcome =>
        val replace = !outcome.contains(true)
        mutex.lock.surround:
          closed.get.ifM(
            IO.unit,
            panels.get.flatMap:
              _.get(refresh.workspace.key) match
                case Some(slot)
                    if slot.revision.eq(refresh.reservation.revision) &&
                      slot.panel.exists(_.token.eq(refresh.entry.token)) =>
                  if refresh.entry.panel.isDisposed then
                    panels.update(_.removed(refresh.workspace.key))
                  else
                    updateEntry(
                      refresh.workspace,
                      slot,
                      refresh.entry,
                      refresh.rendered,
                      if replace then
                        replacePanel(refresh.entry, refresh.rendered)
                      else IO.unit,
                    )
                case _ => IO.unit,
          )

  private def updateEntry(
      workspace: Workspace,
      slot: Slot,
      entry: Entry,
      rendered: RenderedPanel,
      write: IO[Unit],
  ): IO[Unit] =
    val update = write *>
      slot.openIntent.fold(IO.unit)(intent =>
        entry.panel.reveal *>
          consumeOpenIntent(workspace.key, intent),
      )
    update.handleErrorWith:
      case PreviewPanelDisposed =>
        if entry.panel.isDisposed then panels.update(_.removed(workspace.key))
        else
          slot.openIntent match
            case Some(intent) =>
              val empty = slot.copy(panel = None)
              panels.update(
                _.updated(workspace.key, empty),
              ) *>
                create(workspace, empty, rendered, intent)
            case None => panels.update(_.removed(workspace.key))
      case error => IO.raiseError(error)

  private def create(
      workspace: Workspace,
      slot: Slot,
      rendered: RenderedPanel,
      openIntent: Revision,
  ): IO[Unit] =
    val token  = new Object
    val remove = mutex.lock.surround:
      panels
        .modify: current =>
          current.get(workspace.key) match
            case Some(actual)
                if actual.panel.exists(entry => entry.token.eq(token)) =>
              current.removed(workspace.key) -> true
            case _ => current -> false
        .flatMap: removed =>
          if removed then bridges.update(_.removed(workspace.key))
          else IO.unit
    Ref
      .of[IO, Boolean](false)
      .flatMap: keep =>
        def cleanup(panel: PreviewPanel): IO[Unit] =
          panels.update: current =>
            current.get(workspace.key) match
              case Some(actual)
                  if actual.panel.exists(entry => entry.token.eq(token)) =>
                if panel.isDisposed || actual.openIntent.isEmpty then
                  current.removed(workspace.key)
                else
                  current.updated(
                    workspace.key,
                    actual.copy(panel = None),
                  )
              case _ => current
          *> panel.dispose.attempt.void

        Resource
          .make(backend.create(workspace, remove))(panel =>
            keep.get.ifM(IO.unit, cleanup(panel)),
          )
          .use: panel =>
            rendered.installation.flatMap: installation =>
              val entry   = Entry(token, panel, installation.channel)
              val install = panel.setHtml(
                installation.html,
                installation.channel.map(_.token),
              ) *>
                panels.update: current =>
                  current.get(workspace.key) match
                    case Some(actual) if actual.revision.eq(slot.revision) =>
                      current.updated(
                        workspace.key,
                        actual.copy(panel = Some(entry)),
                      )
                    case _ => current
              install *> panel.reveal *>
                consumeOpenIntent(workspace.key, openIntent) *>
                keep.set(true)

  private def replacePanel(
      entry: Entry,
      rendered: RenderedPanel,
  ): IO[Unit] =
    rendered.installation.flatMap: installation =>
      entry.panel.setHtml(
        installation.html,
        installation.channel.map(_.token),
      ) *>
        IO.delay(entry.channel = installation.channel)

  private def consumeOpenIntent(key: String, intent: Revision): IO[Unit] =
    panels.update: current =>
      current.get(key) match
        case Some(slot) if slot.openIntent.exists(_.eq(intent)) =>
          current.updated(key, slot.copy(openIntent = None))
        case _ => current

  private def abandon(
      key: String,
      reservation: Reservation,
  ): IO[Unit] =
    mutex.lock.surround:
      panels.update: current =>
        current.get(key) match
          case Some(slot) =>
            val withoutIntent =
              if reservation.open && slot.openIntent.exists(
                  _.eq(reservation.revision),
                )
              then slot.copy(openIntent = None)
              else slot
            if withoutIntent.revision.eq(reservation.revision) &&
              withoutIntent.panel.isEmpty &&
              withoutIntent.openIntent.isEmpty
            then current.removed(key)
            else current.updated(key, withoutIntent)
          case _ => current

  private def render(
      workspace: Workspace,
      state: PreviewViewState,
  ): IO[RenderedPanel] =
    state match
      case PreviewViewState.Starting =>
        renderStatus(workspace, state)
      case PreviewViewState.Ready(url, supportsLiveRefresh) =>
        renderReady(
          workspace,
          state,
          url,
          allowBridge = supportsLiveRefresh,
          refresh = None,
        )
      case PreviewViewState.Refreshed(url, revision) =>
        renderReady(
          workspace,
          state,
          url,
          allowBridge = true,
          refresh = Some(revision),
        )
      case _ => renderStatus(workspace, state)

  private def renderStatus(
      workspace: Workspace,
      state: PreviewViewState,
  ): IO[RenderedPanel] =
    IO.pure(
      RenderedPanel(
        target = None,
        refresh = None,
        installation = IO.pure(
          PanelInstallation(
            PreviewWebviewHtml.render(workspace, state, None),
            channel = None,
          ),
        ),
      ),
    )

  private def renderReady(
      workspace: Workspace,
      state: PreviewViewState,
      url: PreviewUrl,
      allowBridge: Boolean,
      refresh: Option[SiteRevision],
  ): IO[RenderedPanel] =
    (
      backend.resolve(url),
      resolveBridge(workspace, url, allowBridge),
    ).parTupled
      .flatMap: resolved =>
        IO.fromEither(
          ResolvedPreviewUri
            .parse(resolved._1)
            .leftMap(detail =>
              IllegalArgumentException(
                s"VS Code asExternalUri returned an unusable URI: $detail",
              ),
            ),
        ).flatMap: external =>
          val bridge = resolved._2
            .flatMap(value => ResolvedPreviewUri.parse(value).toOption)
            .filter(candidate =>
              external.cspOrigin.nonEmpty &&
                candidate.cspOrigin == external.cspOrigin,
            )
          val target = bridge.map: bridgeUrl =>
            RefreshTarget(
              previewUrl = external.value,
              bridgeUrl = bridgeUrl.value,
            )
          val installation = IO.delay:
            val channel = target
              .map(value => RefreshChannel(value, NodeCrypto.randomUUID()))
            PanelInstallation(
              PreviewWebviewHtml.render(
                workspace,
                state,
                Some(external),
                bridge,
                channel.map(_.token),
              ),
              channel,
            )
          IO.pure(RenderedPanel(target, refresh, installation))

  private def resolveBridge(
      workspace: Workspace,
      url: PreviewUrl,
      enabled: Boolean,
  ): IO[Option[String]] =
    if !enabled then IO.none
    else
      bridgeSession(workspace.key).flatMap: session =>
        session.cached match
          case Some(cached) if cached.previewUrl == url =>
            IO.pure(cached.resolved)
          case _ =>
            backend
              .resolveBridge(url)
              .handleError(_ => None)
              .flatTap: resolved =>
                bridges.update: current =>
                  current.get(workspace.key) match
                    case Some(actual)
                        if actual.generation.eq(session.generation) =>
                      current.updated(
                        workspace.key,
                        actual.copy(cached = Some(CachedBridge(url, resolved))),
                      )
                    case _ => current

  private def bridgeSession(key: String): IO[BridgeSession] =
    IO.delay(BridgeSession(new Object, cached = None))
      .flatMap: fresh =>
        bridges.modify: current =>
          current.get(key) match
            case Some(existing) => current                     -> existing
            case None           => current.updated(key, fresh) -> fresh

  private def resetBridgeSession(
      key: String,
      state: PreviewViewState,
  ): IO[Unit] =
    state match
      case PreviewViewState.Starting =>
        IO.delay(BridgeSession(new Object, cached = None))
          .flatMap: fresh =>
            bridges.update(_.updated(key, fresh))
      case PreviewViewState.Stopped | PreviewViewState.Failed(_) |
          PreviewViewState.Exited(_) =>
        bridges.update(_.removed(key))
      case PreviewViewState.Ready(_, _) | PreviewViewState.Refreshed(_, _) =>
        IO.unit

  private def message(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString)

  private def closeAll: IO[Unit] =
    mutex.lock
      .surround(
        closed.set(true) *>
          panels.getAndSet(Map.empty).flatTap(_ => bridges.set(Map.empty)),
      )
      .flatMap: current =>
        current.values.toList
          .flatMap(_.panel.toList)
          .traverse(_.panel.dispose.attempt)
          .flatMap: results =>
            results.collect { case Left(error) => error } match
              case first :: remaining =>
                IO.delay(
                  remaining.foreach(first.addSuppressed),
                ) *> IO.raiseError(first)
              case Nil => IO.unit

private[infrastructure] object PreviewPanelRegistry:
  private final class Revision
  private final class Entry(
      val token: Object,
      val panel: PreviewPanel,
      var channel: Option[RefreshChannel],
  )
  private final case class Slot(
      revision: Revision,
      openIntent: Option[Revision],
      panel: Option[Entry],
  )
  private final case class Reservation(revision: Revision, open: Boolean)

  def resource(
      backend: PreviewPanelBackend,
  ): Resource[IO, PreviewPanelRegistry] =
    Resource.make(
      for
        panels  <- Ref.of[IO, Map[String, Slot]](Map.empty)
        bridges <- Ref.of[IO, Map[String, BridgeSession]](Map.empty)
        closed  <- Ref.of[IO, Boolean](false)
        mutex   <- Mutex[IO]
      yield PreviewPanelRegistry(backend, panels, bridges, closed, mutex),
    )(_.closeAll)

private[infrastructure] object PreviewWebviewHtml:
  private[infrastructure] val Styles =
    "html,body{width:100%;height:100%;margin:0;padding:0}" +
      "body{color:var(--vscode-foreground);background:var(--vscode-editor-background);font-family:var(--vscode-font-family)}" +
      "iframe{display:block;width:100%;height:100%;border:0;background:white}" +
      "main{box-sizing:border-box;max-width:48rem;margin:0 auto;padding:3rem 2rem}" +
      "h1{font-size:1.35rem}p{line-height:1.6}"
  private[infrastructure] val StylesHash =
    "'sha256-0Xd2UqyuR101toiMU/5s4npyG081HHWxTpXtf3fr0Ww='"
  private[infrastructure] val Script =
    """(() => {
      |  "use strict";
      |  const frame = document.getElementById("doctrellis-preview");
      |  if (!frame || !frame.dataset.doctrellisPreviewUrl) return;
      |  const directUrl = frame.dataset.doctrellisPreviewUrl;
      |  const bridgeOrigin = frame.dataset.doctrellisBridgeOrigin;
      |  const channelToken = frame.dataset.doctrellisChannelToken;
      |  if (!bridgeOrigin || !channelToken) return;
      |  let active = false;
      |  let legacy = false;
      |  let pendingRefresh = null;
      |  let panelVisible = true;
      |  let timeout = null;
      |  let vscode = null;
      |  const announceReady = () => {
      |    if (!vscode) return;
      |    vscode.postMessage({
      |      protocol: 1,
      |      type: "doctrellis.preview.ready",
      |      channelToken
      |    });
      |  };
      |  const accept = (pending) => {
      |    if (!vscode) return;
      |    vscode.postMessage({
      |      protocol: 1,
      |      type: "doctrellis.preview.refresh-accepted",
      |      refreshId: pending.refreshId,
      |      revision: pending.revision,
      |      channelToken
      |    });
      |  };
      |  const fallback = () => {
      |    if (legacy) return;
      |    clearTimeout(timeout);
      |    timeout = null;
      |    active = false;
      |    legacy = true;
      |    frame.setAttribute("sandbox", "");
      |    frame.removeAttribute("data-doctrellis-preview-url");
      |    frame.src = directUrl;
      |    if (pendingRefresh) {
      |      accept(pendingRefresh);
      |      pendingRefresh = null;
      |    }
      |  };
      |  const armFallback = () => {
      |    clearTimeout(timeout);
      |    timeout = setTimeout(() => {
      |      timeout = null;
      |      if (panelVisible) fallback();
      |      else announceReady();
      |    }, 2000);
      |  };
      |  const hello = () => {
      |    if (!legacy && !active && frame.contentWindow) {
      |      frame.contentWindow.postMessage({
      |        protocol: 1,
      |        type: "doctrellis.bridge.hello"
      |      }, bridgeOrigin);
      |    }
      |  };
      |  armFallback();
      |  try {
      |    vscode = acquireVsCodeApi();
      |  } catch (_) {
      |    fallback();
      |    return;
      |  }
      |  announceReady();
      |  frame.addEventListener("load", hello);
      |  addEventListener("message", (event) => {
      |    const message = event.data;
      |    if (!message || message.protocol !== 1) return;
      |    if (event.source === frame.contentWindow) {
      |      if (event.origin !== bridgeOrigin) return;
      |      if (message.type === "doctrellis.bridge.ready") {
      |        frame.contentWindow.postMessage({
      |          protocol: 1,
      |          type: "doctrellis.bridge.initialize",
      |          url: directUrl
      |        }, bridgeOrigin);
      |      } else if (message.type === "doctrellis.bridge.active") {
      |        active = true;
      |        clearTimeout(timeout);
      |        timeout = null;
      |        if (pendingRefresh) {
      |          frame.contentWindow.postMessage(
      |            pendingRefresh.refresh,
      |            bridgeOrigin
      |          );
      |          accept(pendingRefresh);
      |          pendingRefresh = null;
      |        }
      |      } else if (message.type === "doctrellis.bridge.rejected") {
      |        fallback();
      |      }
      |      return;
      |    }
      |    if (event.source !== parent) return;
      |    if (message.channelToken !== channelToken) return;
      |    if (message.type === "doctrellis.preview.visibility") {
      |      if (typeof message.visible !== "boolean") return;
      |      panelVisible = message.visible;
      |      if (panelVisible && !active && !legacy) {
      |        hello();
      |        armFallback();
      |      }
      |      return;
      |    }
      |    if (message.type !== "doctrellis.preview.refresh" ||
      |        typeof message.refreshId !== "string") return;
      |    const pending = {
      |      refresh: {
      |        protocol: 1,
      |        type: "doctrellis.preview.refresh",
      |        revision: message.revision
      |      },
      |      refreshId: message.refreshId,
      |      revision: message.revision
      |    };
      |    if (legacy) {
      |      frame.src = directUrl;
      |      accept(pending);
      |    } else if (active) {
      |      frame.contentWindow.postMessage(pending.refresh, bridgeOrigin);
      |      accept(pending);
      |    } else pendingRefresh = pending;
      |  });
      |})();""".stripMargin
  private[infrastructure] val ScriptHash =
    s"'sha256-${NodeCrypto.createHash("sha256").update(Script).digest("base64")}'"

  def render(
      workspace: Workspace,
      state: PreviewViewState,
      externalUrl: Option[ResolvedPreviewUri],
      bridgeUrl: Option[ResolvedPreviewUri] = None,
      channelToken: Option[String] = None,
  ): String =
    val name   = workspace.name
    val bridge = (bridgeUrl, channelToken).tupled.flatMap:
      case (url, token) => url.cspOrigin.map(origin => (url, origin, token))
    val body = state match
      case PreviewViewState.Starting =>
        status("Starting preview", s"Starting the preview for $name…")
      case PreviewViewState.Ready(_, _) =>
        externalUrl match
          case Some(url) if url.cspOrigin.nonEmpty =>
            iframe(name, url, bridge, None)
          case Some(_) =>
            status(
              "Preview unavailable",
              "VS Code returned a preview address that cannot be embedded under the preview panel security policy. Use Open in External Browser instead.",
            )
          case None =>
            status("Preview unavailable", "The preview URL is unavailable.")
      case PreviewViewState.Refreshed(_, revision) =>
        externalUrl match
          case Some(url) if url.cspOrigin.nonEmpty =>
            iframe(name, url, bridge, Some(revision))
          case Some(_) =>
            status(
              "Preview unavailable",
              "VS Code returned a preview address that cannot be embedded under the preview panel security policy. Use Open in External Browser instead.",
            )
          case None =>
            status("Preview unavailable", "The preview URL is unavailable.")
      case PreviewViewState.Stopped =>
        status(
          "Preview stopped",
          s"The preview connection for $name has ended. Start or restart the preview to reconnect.",
        )
      case PreviewViewState.Failed(reason) =>
        status(
          "Preview failed",
          s"The preview for $name could not start: $reason",
        )
      case PreviewViewState.Exited(exit) =>
        status(
          "Preview disconnected",
          s"The preview process for $name exited unexpectedly (${exit.render}). Restart the preview to reconnect.",
        )

    val frameOrigins = (externalUrl.toList ++ bridge.toList.map(_._1))
      .flatMap(_.cspOrigin)
      .distinct
    val frameSource =
      if frameOrigins.isEmpty then "frame-src 'none';"
      else s"frame-src ${frameOrigins.map(escape).mkString(" ")};"
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |<meta charset="UTF-8">
       |<meta http-equiv="Content-Security-Policy" content="default-src 'none'; $frameSource script-src $ScriptHash; style-src $StylesHash;">
       |<meta name="viewport" content="width=device-width, initial-scale=1.0">
       |<title>DocTrellis Preview — ${escape(name)}</title>
       |<style>$Styles</style>
       |</head>
       |<body>$body<script>$Script</script></body>
       |</html>""".stripMargin

  private def iframe(
      workspaceName: String,
      url: ResolvedPreviewUri,
      bridge: Option[(ResolvedPreviewUri, String, String)],
      revision: Option[SiteRevision],
  ): String =
    val safeUrl           = escape(url.value)
    val safeName          = escape(workspaceName)
    val revisionAttribute = revision
      .map(value => s" data-doctrellis-revision=\"${value.value}\"")
      .getOrElse("")
    bridge match
      case Some((bridgeUrl, bridgeOrigin, channelToken)) =>
        val safeBridge = escape(bridgeUrl.value)
        val safeOrigin = escape(bridgeOrigin)
        val safeToken  = escape(channelToken)
        s"""<iframe id="doctrellis-preview" src="$safeBridge" data-doctrellis-preview-url="$safeUrl" data-doctrellis-bridge-origin="$safeOrigin" data-doctrellis-channel-token="$safeToken" title="DocTrellis Preview — $safeName"$revisionAttribute sandbox="allow-scripts allow-same-origin" referrerpolicy="no-referrer"></iframe>"""
      case None =>
        s"""<iframe src="$safeUrl" title="DocTrellis Preview — $safeName"$revisionAttribute sandbox="" referrerpolicy="no-referrer"></iframe>"""

  private def status(title: String, detail: String): String =
    s"<main><h1>${escape(title)}</h1><p>${escape(detail)}</p></main>"

  private[infrastructure] def escape(value: String): String =
    value
      .replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")
      .replace("'", "&#39;")

private[infrastructure] object PreviewVisibilityDelivery:
  def post(
      snapshot: IO[Option[Boolean]],
      send: Boolean => IO[Boolean],
      shouldLog: IO[Boolean],
      log: String => IO[Unit],
  ): IO[Option[Boolean]] =
    snapshot.attempt.flatMap:
      case Right(None)          => IO.none
      case Right(Some(visible)) =>
        send(visible).attempt.flatMap:
          case Right(true)  => IO.pure(Some(visible))
          case Right(false) =>
            safelyLog(
              shouldLog,
              log,
              "DocTrellis preview visibility update was not delivered by VS Code.\n",
            ).as(None)
          case Left(error) =>
            safelyLog(
              shouldLog,
              log,
              s"Failed to deliver DocTrellis preview visibility: ${message(error)}\n",
            ).as(None)
      case Left(error) =>
        safelyLog(
          shouldLog,
          log,
          s"Failed to read DocTrellis preview visibility: ${message(error)}\n",
        ).as(None)

  private def safelyLog(
      shouldLog: IO[Boolean],
      log: String => IO[Unit],
      value: String,
  ): IO[Unit] =
    shouldLog.attempt.flatMap:
      case Right(true)            => log(value).handleError(_ => ())
      case Right(false) | Left(_) => IO.unit

  private def message(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString)

private[infrastructure] final class PreviewVisibilityRelay private (
    mutex: Mutex[IO],
    lastDelivered: Ref[IO, Option[(String, Boolean)]],
):
  def post(
      channelToken: String,
      force: Boolean,
      snapshot: IO[Option[Boolean]],
      send: Boolean => IO[Boolean],
      shouldLog: IO[Boolean],
      log: String => IO[Unit],
  ): IO[Unit] =
    mutex.lock.surround:
      PreviewVisibilityDelivery
        .post(
          snapshot,
          visible =>
            lastDelivered.get.flatMap:
              case Some((token, previous))
                  if !force && token == channelToken && previous == visible =>
                IO.pure(true)
              case _ => send(visible),
          shouldLog,
          log,
        )
        .flatMap(
          _.traverse_(visible =>
            lastDelivered.set(Some(channelToken -> visible)),
          ),
        )

private[infrastructure] object PreviewVisibilityRelay:
  def create: IO[PreviewVisibilityRelay] =
    (Mutex[IO], Ref.of[IO, Option[(String, Boolean)]](None)).mapN(
      new PreviewVisibilityRelay(_, _),
    )

private[infrastructure] final class VscodePreviewPanelBackend(
    dispatcher: Dispatcher[IO],
    log: String => IO[Unit],
) extends PreviewPanelBackend:
  def resolve(url: PreviewUrl): IO[String] =
    IO.fromPromise(IO.delay(VSEnv.asExternalUri(VSUri.parse(url.value))))
      .map(_.toString())

  override def resolveBridge(url: PreviewUrl): IO[Option[String]] =
    IO.fromEither(
      PreviewWebviewBridge
        .url(url)
        .leftMap(detail => IllegalArgumentException(detail)),
    ).flatMap: bridge =>
      NodeHttpBridgeProbe
        .available(bridge)
        .ifM(resolve(bridge).map(Some(_)), IO.none)

  def create(
      workspace: Workspace,
      onDispose: IO[Unit],
  ): IO[PreviewPanel] =
    (
      RefreshAcknowledgements.create(),
      PreviewVisibilityRelay.create,
    ).tupled
      .flatMap: (acknowledgements, visibilityRelay) =>
        IO.delay:
          val showOptions = js.Dynamic.literal(
            viewColumn = -1,
            preserveFocus = false,
          )
          val panel = VSWindow.createWebviewPanel(
            "doctrellis.preview",
            s"DocTrellis Preview — ${workspace.name}",
            showOptions,
            PreviewWebviewOptions.value,
          )
          var disposed                              = false
          var installedChannelToken: Option[String] = None
          var registrations: List[Disposable]       = Nil
          def postVisibility(
              expectedToken: String,
              force: Boolean,
          ): IO[Unit] =
            visibilityRelay.post(
              channelToken = expectedToken,
              force = force,
              snapshot = IO.delay(
                Option.when(
                  !disposed && installedChannelToken.contains(expectedToken),
                )(panel.visible),
              ),
              send = visible =>
                IO.fromPromise(
                  IO.delay(
                    panel.webview.postMessage(
                      js.Dynamic.literal(
                        protocol = 1,
                        `type` = "doctrellis.preview.visibility",
                        visible = visible,
                        channelToken = expectedToken,
                      ),
                    ),
                  ),
                ),
              shouldLog = IO.delay(
                !disposed && installedChannelToken.contains(expectedToken),
              ),
              log = log,
            )
          def releaseRegistrations(): Unit =
            val current = registrations
            registrations = Nil
            current.foreach(_.dispose())
          val listener: js.Function0[Unit] =
            () =>
              disposed = true
              // VS Code permits removing the currently dispatching listener.
              releaseRegistrations()
              dispatcher.unsafeRunAndForget(
                acknowledgements.cancel *> onDispose,
              )
          val messageListener: js.Function1[js.Any, Unit] = message =>
            PreviewWebviewMessage
              .refreshAcknowledgement(message)
              .foreach: acknowledgement =>
                dispatcher.unsafeRunAndForget(
                  acknowledgements.accept(acknowledgement),
                )
            PreviewWebviewMessage
              .wrapperReady(message)
              .foreach: channelToken =>
                dispatcher.unsafeRunAndForget(
                  postVisibility(channelToken, force = true),
                )
          val viewStateListener: js.Function1[
            WebviewPanelOnDidChangeViewStateEvent,
            Unit,
          ] = _ =>
            installedChannelToken.foreach: channelToken =>
              dispatcher.unsafeRunAndForget(
                postVisibility(channelToken, force = false),
              )
          val disposeRegistration   = panel.onDidDispose(listener)
          val viewStateRegistration = panel.onDidChangeViewState(
            viewStateListener,
          )
          val messageRegistration = panel.webview.onDidReceiveMessage(
            messageListener,
          )
          registrations = List(
            messageRegistration,
            viewStateRegistration,
            disposeRegistration,
          )
          new PreviewPanel:
            def isDisposed: Boolean = disposed

            def setHtml(
                value: String,
                channelToken: Option[String],
            ): IO[Unit] =
              IO.delay:
                if disposed then throw PreviewPanelDisposed
                else
                  installedChannelToken = channelToken
                  panel.webview.html = value

            override def refresh(
                revision: SiteRevision,
                channelToken: String,
            ): IO[Boolean] =
              if disposed then IO.raiseError(PreviewPanelDisposed)
              else
                IO.delay:
                  RefreshAcknowledgement(
                    NodeCrypto.randomUUID(),
                    revision.value.toString,
                    channelToken,
                  )
                .flatMap: acknowledgement =>
                    acknowledgements.await(acknowledgement):
                      IO.fromPromise(
                        IO.delay(
                          panel.webview.postMessage(
                            js.Dynamic.literal(
                              protocol = 1,
                              `type` = "doctrellis.preview.refresh",
                              refreshId = acknowledgement.refreshId,
                              revision = acknowledgement.revision,
                              channelToken = acknowledgement.channelToken,
                            ),
                          ),
                        ),
                      )

            def reveal: IO[Unit] =
              IO.delay:
                if disposed then throw PreviewPanelDisposed
                else panel.reveal()

            def dispose: IO[Unit] =
              (acknowledgements.cancel *>
                IO.delay:
                  if !disposed then
                    disposed = true
                    panel.dispose()
              ).guarantee(
                IO.delay(releaseRegistrations()),
              )

private[infrastructure] final case class RefreshAcknowledgement(
    refreshId: String,
    revision: String,
    channelToken: String,
)

private final case class PendingRefresh(
    id: Object,
    completion: Deferred[IO, Boolean],
)

private[infrastructure] final class RefreshAcknowledgements private (
    pending: Ref[IO, Map[RefreshAcknowledgement, PendingRefresh]],
    timeout: FiniteDuration,
):
  def await(
      acknowledgement: RefreshAcknowledgement,
  )(send: IO[Boolean]): IO[Boolean] =
    Deferred[IO, Boolean].flatMap: completion =>
      val refresh = PendingRefresh(new Object, completion)
      (pending.update(_.updated(acknowledgement, refresh)) *>
        send.flatMap: delivered =>
          if !delivered then IO.pure(false)
          else completion.get.timeoutTo(timeout, IO.pure(false)))
        .guarantee(clear(acknowledgement, refresh.id))

  def accept(acknowledgement: RefreshAcknowledgement): IO[Unit] =
    pending
      .modify: current =>
        current.removed(acknowledgement) ->
          current.get(acknowledgement).map(_.completion)
      .flatMap(_.traverse_(_.complete(true).void))

  def cancel: IO[Unit] =
    pending
      .getAndSet(Map.empty)
      .flatMap:
        _.values.toList.traverse_(_.completion.complete(false).void)

  private def clear(
      acknowledgement: RefreshAcknowledgement,
      id: Object,
  ): IO[Unit] =
    // Keep a reused key's newer waiter intact if an older waiter finishes late.
    pending.update:
      case current if current.get(acknowledgement).exists(_.id.eq(id)) =>
        current.removed(acknowledgement)
      case current => current

private[infrastructure] object RefreshAcknowledgements:
  def create(
      timeout: FiniteDuration = PreviewWebviewMessage.AcknowledgementTimeout,
  ): IO[RefreshAcknowledgements] =
    Ref
      .of[IO, Map[RefreshAcknowledgement, PendingRefresh]](Map.empty)
      .map(new RefreshAcknowledgements(_, timeout))

private[infrastructure] object PreviewWebviewMessage:
  val AcknowledgementTimeout: FiniteDuration = 2.seconds

  def refreshAcknowledgement(
      value: js.Any,
  ): Option[RefreshAcknowledgement] =
    Option
      .when(
        value != null && js.typeOf(value) == "object" &&
          !js.Array.isArray(value),
      )(value.asInstanceOf[js.Dynamic])
      .flatMap: message =>
        for
          protocol     <- numberField(message, "protocol")
          messageType  <- stringField(message, "type")
          refreshId    <- stringField(message, "refreshId")
          revision     <- stringField(message, "revision")
          channelToken <- stringField(message, "channelToken")
          if protocol == 1.0 &&
            messageType == "doctrellis.preview.refresh-accepted"
        yield RefreshAcknowledgement(refreshId, revision, channelToken)

  def wrapperReady(value: js.Any): Option[String] =
    Option
      .when(
        value != null && js.typeOf(value) == "object" &&
          !js.Array.isArray(value),
      )(value.asInstanceOf[js.Dynamic])
      .flatMap: message =>
        for
          protocol     <- numberField(message, "protocol")
          messageType  <- stringField(message, "type")
          channelToken <- stringField(message, "channelToken")
          if protocol == 1.0 && messageType == "doctrellis.preview.ready"
        yield channelToken

  private def numberField(value: js.Dynamic, name: String): Option[Double] =
    val field = value.selectDynamic(name)
    Option.when(!js.isUndefined(field) && js.typeOf(field) == "number"):
      field.asInstanceOf[Double]

  private def stringField(value: js.Dynamic, name: String): Option[String] =
    val field = value.selectDynamic(name)
    Option.when(!js.isUndefined(field) && js.typeOf(field) == "string"):
      field.asInstanceOf[String]

private[infrastructure] object PreviewWebviewOptions:
  def value: js.Object =
    js.Dynamic.literal(
      enableScripts = true,
      enableCommandUris = false,
      localResourceRoots = js.Array(),
      retainContextWhenHidden = true,
    )

private[infrastructure] object VscodeExternalPreview:
  private val externalPreview = ExternalPreview[Uri](
    url =>
      IO.fromPromise(IO.delay(VSEnv.asExternalUri(VSUri.parse(url.value)))),
    value => IO.fromPromise(IO.delay(VSEnv.openExternal(value))),
  )

  def open(url: PreviewUrl): IO[Unit] = externalPreview.open(url)

private[infrastructure] final class ExternalPreview[A](
    resolve: PreviewUrl => IO[A],
    openResolved: A => IO[Boolean],
):
  def open(url: PreviewUrl): IO[Unit] =
    resolve(url).flatMap: resolved =>
      openResolved(resolved).flatMap: opened =>
        IO.raiseUnless(opened)(
          IllegalStateException(
            "VS Code could not open the DocTrellis preview externally.",
          ),
        )
