package org.doctrellis.vscode.infrastructure

import cats.effect.std.Dispatcher
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import org.doctrellis.vscode.application.PreviewService
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

private[infrastructure] final class VscodeOutput private (
    channel: OutputChannel,
):
  def append(value: String): IO[Unit] = IO.delay(channel.append(value))

  def info(message: String): IO[Unit] =
    append(s"$message\n") *>
      IO.fromPromise(IO.delay(VSWindow.showInformationMessage(message)))
        .attempt
        .start
        .void

  def report(message: String): IO[Unit] =
    append(s"ERROR: $message\n") *>
      IO.delay(channel.show(true)) *>
      IO.fromPromise(
        IO.delay(VSWindow.showErrorMessage(s"DocTrellis: $message")),
      ).void

  def show: IO[Unit] = IO.delay(channel.show(false))

private[infrastructure] object VscodeOutput:
  def resource: Resource[IO, VscodeOutput] =
    Resource
      .make(IO.delay(VSWindow.createOutputChannel("DocTrellis")))(channel =>
        IO.delay(channel.dispose()),
      )
      .map(VscodeOutput(_))

private[infrastructure] object VscodePreferences:
  def load(workspace: Workspace): IO[PreviewPolicy] =
    IO.delay:
      val settings =
        VSWorkspace.getConfiguration("doctrellis", VSUri.parse(workspace.uri))
      val open     = settings.get("openPreviewWhenReady", true)
      val interval = settings.get("pollIntervalMs", 500)
      PreviewPolicy
        .parse(open, interval)
        .leftMap(problem => IllegalArgumentException(problem.message))
    .flatMap(IO.fromEither)

private[infrastructure] object VscodePreview:
  def open(url: PreviewUrl): IO[Unit] =
    val original = VSUri.parse(url.value)
    IO.fromPromise(IO.delay(VSEnv.asExternalUri(original)))
      .attempt
      .flatMap:
        case Right(resolved) =>
          openInsideVscode(resolved)
            .handleErrorWith(_ => openExternal(resolved))
        case Left(_) => openExternal(original)

  private def openInsideVscode(uri: Uri): IO[Unit] =
    IO.fromPromise(
      IO.delay(VSCommands.executeCommand[js.Any]("vscode.open", uri)),
    ).void

  private def openExternal(uri: Uri): IO[Unit] =
    IO.fromPromise(IO.delay(VSEnv.openExternal(uri)))
      .flatMap: opened =>
        IO.raiseUnless(opened)(
          IllegalStateException(
            "VS Code could not open the DocTrellis preview.",
          ),
        )

private[infrastructure] object WorkspaceSelection:
  def choose: IO[Option[Workspace]] =
    IO.delay(
      VSWorkspace.workspaceFolders.toOption
        .map(_.toVector)
        .getOrElse(Vector.empty),
    ).flatMap:
      case Vector() =>
        IO.fromPromise(
          IO.delay(
            VSWindow
              .showErrorMessage("DocTrellis requires an open workspace folder."),
          ),
        ).as(None)
      case Vector(folder) => IO.pure(Some(fromFolder(folder)))
      case folders        =>
        val labels =
          folders.map(folder => s"${folder.name} — ${folder.uri.fsPath}")
        val options = js.Dynamic.literal(
          placeHolder = "Select the workspace folder for DocTrellis preview",
          title = "DocTrellis",
        )
        IO.fromPromise(
          IO.delay(VSWindow.showQuickPick(labels.toJSArray, options)),
        ).map: selected =>
          selected.toOption.flatMap: label =>
            labels.indexOf(label) match
              case -1    => None
              case index => Some(fromFolder(folders(index)))

  def fromFolder(folder: WorkspaceFolder): Workspace =
    val uri = folder.uri.toString()
    Workspace(uri, folder.name, folder.uri.fsPath, uri)

private[infrastructure] object CommandBindings:
  def resource(
      context: ExtensionContext,
      dispatcher: Dispatcher[IO],
      service: PreviewService,
      output: VscodeOutput,
  ): Resource[IO, Unit] =
    List(
      workspaceCommand(
        context,
        dispatcher,
        output,
        "doctrellis.startPreview",
        requireTrust = true,
      )(service.start),
      workspaceCommand(
        context,
        dispatcher,
        output,
        "doctrellis.stopPreview",
        requireTrust = false,
      )(service.stop),
      workspaceCommand(
        context,
        dispatcher,
        output,
        "doctrellis.restartPreview",
        requireTrust = true,
      )(service.restart),
      workspaceCommand(
        context,
        dispatcher,
        output,
        "doctrellis.openPreview",
        requireTrust = true,
      )(service.open),
      simpleCommand(context, dispatcher, output, "doctrellis.showOutput")(
        output.show,
      ),
      workspaceListener(context, dispatcher, service, output),
    ).sequence_.void

  private def workspaceCommand(
      context: ExtensionContext,
      dispatcher: Dispatcher[IO],
      output: VscodeOutput,
      id: String,
      requireTrust: Boolean,
  )(action: Workspace => IO[Unit]): Resource[IO, Unit] =
    val callback: js.Function0[js.Any] = () =>
      dispatcher
        .unsafeToPromise(
          trusted(requireTrust) *> WorkspaceSelection.choose
            .flatMap(_.traverse_(action))
            .handleErrorWith(error => report(output, error)),
        )
        .asInstanceOf[js.Any]
    disposable(context, IO.delay(VSCommands.registerCommand(id, callback)))

  private def simpleCommand(
      context: ExtensionContext,
      dispatcher: Dispatcher[IO],
      output: VscodeOutput,
      id: String,
  )(action: IO[Unit]): Resource[IO, Unit] =
    val callback: js.Function0[js.Any] = () =>
      dispatcher
        .unsafeToPromise(action.handleErrorWith(error => report(output, error)))
        .asInstanceOf[js.Any]
    disposable(context, IO.delay(VSCommands.registerCommand(id, callback)))

  private def workspaceListener(
      context: ExtensionContext,
      dispatcher: Dispatcher[IO],
      service: PreviewService,
      output: VscodeOutput,
  ): Resource[IO, Unit] =
    val listener: js.Function1[WorkspaceFoldersChangeEvent, Unit] = event =>
      val stop = event.removed.toList
        .traverse_(folder =>
          service.stopSilently(WorkspaceSelection.fromFolder(folder)),
        )
        .handleErrorWith(error => report(output, error))
      dispatcher.unsafeRunAndForget(stop)
    disposable(
      context,
      IO.delay(VSWorkspace.onDidChangeWorkspaceFolders(listener)),
    )

  private def trusted(required: Boolean): IO[Unit] =
    IO.delay(VSWorkspace.isTrusted)
      .flatMap: trusted =>
        IO.raiseUnless(!required || trusted)(
          IllegalStateException(
            "DocTrellis cannot run preview commands in an untrusted workspace.",
          ),
        )

  private def disposable(
      context: ExtensionContext,
      acquire: IO[Disposable],
  ): Resource[IO, Unit] =
    Resource
      .make(
        acquire.flatTap(disposable =>
          IO.delay(context.subscriptions.push(disposable)).void,
        ),
      )(disposable => IO.delay(disposable.dispose()))
      .void

  private def report(output: VscodeOutput, error: Throwable): IO[Unit] =
    output
      .report(
        Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString),
      )
      .handleError(_ => ())
