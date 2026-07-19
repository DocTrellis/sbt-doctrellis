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
      VscodePreferencesParser
        .parse(open, interval)
        .leftMap(problem => IllegalArgumentException(problem.message))
    .flatMap(IO.fromEither)

private[infrastructure] object VscodePreferencesParser:
  private val OpenPath     = "doctrellis.openPreviewWhenReady"
  private val IntervalPath = "doctrellis.pollIntervalMs"

  def parse(
      openWhenReady: js.Any,
      pollIntervalMs: js.Any,
  ): Either[ConfigProblem, PreviewPolicy] =
    for
      open     <- boolean(openWhenReady, OpenPath)
      interval <- integer(pollIntervalMs, IntervalPath)
      policy   <- PreviewPolicy.parse(open, interval)
    yield policy

  private def boolean(
      value: js.Any,
      path: String,
  ): Either[ConfigProblem, Boolean] =
    Either.cond(
      js.typeOf(value) == "boolean",
      value.asInstanceOf[Boolean],
      ConfigProblem.Expected(path, "a boolean"),
    )

  private def integer(
      value: js.Any,
      path: String,
  ): Either[ConfigProblem, Int] =
    if js.typeOf(value) != "number" then
      Left(ConfigProblem.Expected(path, "an integer"))
    else
      val number = value.asInstanceOf[Double]
      Either.cond(
        number.isFinite && number.isWhole &&
          number >= Int.MinValue && number <= Int.MaxValue,
        number.toInt,
        ConfigProblem.Expected(path, "an integer"),
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
        val candidates       = WorkspaceOptions.from(folders.map(fromFolder))
        val labels           = candidates.map(_._1)
        val quickPickOptions = js.Dynamic.literal(
          placeHolder = "Select the workspace folder for DocTrellis preview",
          title = "DocTrellis",
        )
        IO.fromPromise(
          IO.delay(VSWindow.showQuickPick(labels.toJSArray, quickPickOptions)),
        ).map: selected =>
          selected.toOption.flatMap: label =>
            WorkspaceOptions.select(candidates, label)

  def fromFolder(folder: WorkspaceFolder): Workspace =
    val uri = folder.uri.toString()
    Workspace(uri, folder.name, folder.uri.fsPath, uri)

private[infrastructure] object CommandBindings:
  def resource(
      context: ExtensionContext,
      dispatcher: Dispatcher[IO],
      service: PreviewService[IO],
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
      workspaceCommand(
        context,
        dispatcher,
        output,
        "doctrellis.openExternalPreview",
        requireTrust = true,
      )(service.openExternal),
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
      service: PreviewService[IO],
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
      .flatMap(trusted =>
        IO.fromEither(WorkspaceTrust.validate(required, trusted)),
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
