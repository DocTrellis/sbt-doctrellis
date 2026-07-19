package org.doctrellis.vscode.infrastructure

import scala.scalajs.js

import cats.effect.{IO, Ref, Resource}
import cats.effect.std.{Dispatcher, Mutex}
import cats.syntax.all.*

import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

private[infrastructure] trait PreviewPanel:
  def setHtml(value: String): IO[Unit]
  def reveal: IO[Unit]
  def dispose: IO[Unit]

private[infrastructure] case object PreviewPanelDisposed
    extends RuntimeException("The DocTrellis preview panel was disposed.")

private[infrastructure] trait PreviewPanelBackend:
  def resolve(url: PreviewUrl): IO[String]
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

private[infrastructure] final class PreviewPanelRegistry private (
    backend: PreviewPanelBackend,
    panels: Ref[IO, Map[String, PreviewPanelRegistry.Slot]],
    closed: Ref[IO, Boolean],
    mutex: Mutex[IO],
):
  import PreviewPanelRegistry.{Entry, Reservation, Revision, Slot}

  def show(workspace: Workspace, state: PreviewViewState): IO[Unit] =
    display(workspace, state, open = true)

  def update(workspace: Workspace, state: PreviewViewState): IO[Unit] =
    display(workspace, state, open = false)

  private def display(
      workspace: Workspace,
      state: PreviewViewState,
      open: Boolean,
  ): IO[Unit] =
    reserve(workspace.key, open).flatMap:
      case None              => IO.unit
      case Some(reservation) =>
        render(workspace, state).attempt
          .flatMap:
            case Right(html) => commit(workspace, reservation, html)
            case Left(error) =>
              val fallback = PreviewWebviewHtml.render(
                workspace,
                PreviewViewState.Failed(message(error)),
                None,
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
            case Some(slot) =>
              val revision = new Revision
              val updated  = slot.copy(
                revision = revision,
                visible = slot.visible || open,
              )
              current.updated(key, updated) ->
                Some(Reservation(revision, open))
            case None if open =>
              val revision = new Revision
              current.updated(key, Slot(revision, visible = true, None)) ->
                Some(Reservation(revision, open = true))
            case None => current -> None,
      )

  private def commit(
      workspace: Workspace,
      reservation: Reservation,
      html: String,
  ): IO[Unit] =
    mutex.lock.surround:
      closed.get.ifM(
        IO.unit,
        panels.get.flatMap:
          _.get(workspace.key) match
            case Some(slot) if slot.revision.eq(reservation.revision) =>
              slot.panel match
                case Some(entry) =>
                  val write = entry.panel.setHtml(html) *>
                    IO.whenA(reservation.open)(entry.panel.reveal)
                  write.handleErrorWith:
                    case PreviewPanelDisposed =>
                      if reservation.open then
                        panels.update(
                          _.updated(workspace.key, slot.copy(panel = None)),
                        ) *>
                          create(workspace, slot.copy(panel = None), html)
                      else panels.update(_.removed(workspace.key))
                    case error => IO.raiseError(error)
                case None if slot.visible => create(workspace, slot, html)
                case None                 => IO.unit
            case Some(slot) if reservation.open =>
              slot.panel.fold(IO.unit)(entry =>
                entry.panel.reveal.handleErrorWith:
                  case PreviewPanelDisposed => IO.unit
                  case error                => IO.raiseError(error),
              )
            case _ => IO.unit,
      )

  private def create(
      workspace: Workspace,
      slot: Slot,
      html: String,
  ): IO[Unit] =
    val token  = new Object
    val remove = mutex.lock.surround:
      panels.update: current =>
        current.get(workspace.key) match
          case Some(actual)
              if actual.panel.exists(entry => entry.token.eq(token)) =>
            current.removed(workspace.key)
          case _ => current
    Ref
      .of[IO, Boolean](false)
      .flatMap: keep =>
        def cleanup(panel: PreviewPanel): IO[Unit] =
          panels.update: current =>
            current.get(workspace.key) match
              case Some(actual)
                  if actual.panel.exists(entry => entry.token.eq(token)) =>
                current.removed(workspace.key)
              case _ => current
          *> panel.dispose.attempt.void

        Resource
          .make(backend.create(workspace, remove))(panel =>
            keep.get.ifM(IO.unit, cleanup(panel)),
          )
          .use: panel =>
            val entry   = Entry(token, panel)
            val install = panel.setHtml(html) *>
              panels.update: current =>
                current.get(workspace.key) match
                  case Some(actual) if actual.revision.eq(slot.revision) =>
                    current.updated(
                      workspace.key,
                      actual.copy(panel = Some(entry)),
                    )
                  case _ => current
            install *> panel.reveal *> keep.set(true)

  private def abandon(
      key: String,
      reservation: Reservation,
  ): IO[Unit] =
    mutex.lock.surround:
      panels.update: current =>
        current.get(key) match
          case Some(slot)
              if slot.revision.eq(reservation.revision) && slot.panel.isEmpty =>
            current.removed(key)
          case _ => current

  private def render(
      workspace: Workspace,
      state: PreviewViewState,
  ): IO[String] =
    state match
      case PreviewViewState.Ready(url) =>
        renderReady(workspace, state, url)
      case PreviewViewState.Refreshed(url, _) =>
        renderReady(workspace, state, url)
      case _ => IO.pure(PreviewWebviewHtml.render(workspace, state, None))

  private def renderReady(
      workspace: Workspace,
      state: PreviewViewState,
      url: PreviewUrl,
  ): IO[String] =
    backend
      .resolve(url)
      .flatMap: resolved =>
        IO.fromEither(
          ResolvedPreviewUri
            .parse(resolved)
            .leftMap(detail =>
              IllegalArgumentException(
                s"VS Code asExternalUri returned an unusable URI: $detail",
              ),
            ),
        ).map(external =>
          PreviewWebviewHtml.render(workspace, state, Some(external)),
        )

  private def message(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString)

  private def closeAll: IO[Unit] =
    mutex.lock
      .surround(closed.set(true) *> panels.getAndSet(Map.empty))
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
  private final case class Entry(token: Object, panel: PreviewPanel)
  private final case class Slot(
      revision: Revision,
      visible: Boolean,
      panel: Option[Entry],
  )
  private final case class Reservation(revision: Revision, open: Boolean)

  def resource(
      backend: PreviewPanelBackend,
  ): Resource[IO, PreviewPanelRegistry] =
    Resource.make(
      for
        panels <- Ref.of[IO, Map[String, Slot]](Map.empty)
        closed <- Ref.of[IO, Boolean](false)
        mutex  <- Mutex[IO]
      yield PreviewPanelRegistry(backend, panels, closed, mutex),
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

  def render(
      workspace: Workspace,
      state: PreviewViewState,
      externalUrl: Option[ResolvedPreviewUri],
  ): String =
    val name = workspace.name
    val body = state match
      case PreviewViewState.Starting =>
        status("Starting preview", s"Starting the preview for $name…")
      case PreviewViewState.Ready(_) =>
        externalUrl match
          case Some(url) if url.cspOrigin.nonEmpty => iframe(name, url, None)
          case Some(_)                             =>
            status(
              "Preview unavailable",
              "VS Code returned a preview address that cannot be embedded under the preview panel security policy. Use Open in External Browser instead.",
            )
          case None =>
            status("Preview unavailable", "The preview URL is unavailable.")
      case PreviewViewState.Refreshed(_, revision) =>
        externalUrl match
          case Some(url) if url.cspOrigin.nonEmpty =>
            iframe(name, url, Some(revision))
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

    val frameSource = externalUrl
      .flatMap(_.cspOrigin)
      .map(origin => s"frame-src ${escape(origin)};")
      .getOrElse("frame-src 'none';")
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |<meta charset="UTF-8">
       |<meta http-equiv="Content-Security-Policy" content="default-src 'none'; $frameSource style-src $StylesHash;">
       |<meta name="viewport" content="width=device-width, initial-scale=1.0">
       |<title>DocTrellis Preview — ${escape(name)}</title>
       |<style>$Styles</style>
       |</head>
       |<body>$body</body>
       |</html>""".stripMargin

  private def iframe(
      workspaceName: String,
      url: ResolvedPreviewUri,
      revision: Option[SiteRevision],
  ): String =
    val safeUrl           = escape(url.value)
    val safeName          = escape(workspaceName)
    val revisionAttribute = revision
      .map(value => s" data-doctrellis-revision=\"${value.value}\"")
      .getOrElse("")
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

private[infrastructure] final class VscodePreviewPanelBackend(
    dispatcher: Dispatcher[IO],
) extends PreviewPanelBackend:
  def resolve(url: PreviewUrl): IO[String] =
    IO.fromPromise(IO.delay(VSEnv.asExternalUri(VSUri.parse(url.value))))
      .map(_.toString())

  def create(
      workspace: Workspace,
      onDispose: IO[Unit],
  ): IO[PreviewPanel] =
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
      var disposed                     = false
      val listener: js.Function0[Unit] =
        () =>
          disposed = true
          dispatcher.unsafeRunAndForget(onDispose)
      val registration = panel.onDidDispose(listener)
      new PreviewPanel:
        def setHtml(value: String): IO[Unit] =
          IO.delay:
            if disposed then throw PreviewPanelDisposed
            else panel.webview.html = value

        def reveal: IO[Unit] =
          IO.delay:
            if disposed then throw PreviewPanelDisposed
            else panel.reveal()

        def dispose: IO[Unit] =
          IO.delay:
            if !disposed then
              disposed = true
              panel.dispose()
          .guarantee(IO.delay(registration.dispose()))

private[infrastructure] object PreviewWebviewOptions:
  def value: js.Object =
    js.Dynamic.literal(
      enableScripts = false,
      enableCommandUris = false,
      localResourceRoots = js.Array(),
      retainContextWhenHidden = false,
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
