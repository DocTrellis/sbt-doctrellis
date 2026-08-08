package org.doctrellis.vscode.infrastructure

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

import cats.effect.{IO, Resource}
import cats.effect.std.{Dispatcher, Supervisor}
import cats.syntax.all.*

import org.doctrellis.vscode.application.{PreviewService, ThrowableMessage}
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.*

private[infrastructure] final class VscodeOutput private (
    channel: OutputChannel,
    backgroundTasks: BackgroundTasks,
):
  def append(value: String): IO[Unit] = IO.delay(channel.append(value))

  def info(message: String): IO[Unit] =
    append(s"$message\n") *>
      backgroundTasks
        .submit(
          IO.fromPromise(IO.delay(VSWindow.showInformationMessage(message)))
            .attempt
            .void,
        )

  def report(message: String): IO[Unit] =
    append(s"ERROR: $message\n") *>
      IO.delay(channel.show(true)) *>
      IO.fromPromise(
        IO.delay(VSWindow.showErrorMessage(s"DocTrellis: $message")),
      ).void

  def show: IO[Unit] = IO.delay(channel.show(false))

private[infrastructure] object VscodeOutput:
  def resource: Resource[IO, VscodeOutput] =
    for
      channel <- Resource.make(
        IO.delay(VSWindow.createOutputChannel("DocTrellis")),
      )(channel => IO.delay(channel.dispose()))
      backgroundTasks <- BackgroundTasks.resource
    yield VscodeOutput(channel, backgroundTasks)

private[infrastructure] final class BackgroundTasks private (
    supervisor: Supervisor[IO],
):
  def submit(task: IO[Unit]): IO[Unit] = supervisor.supervise(task).void

private[infrastructure] object BackgroundTasks:
  def resource: Resource[IO, BackgroundTasks] =
    Supervisor[IO](await = false).map(BackgroundTasks(_))

private[infrastructure] object VscodePreferences:
  def load(workspace: Workspace): IO[PreviewPolicy] =
    IO.delay:
      val settings =
        VSWorkspace.getConfiguration(
          "doctrellis",
          VSUri.parse(workspace.key.value),
        )
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
  def choose(output: VscodeOutput): IO[Option[Workspace]] =
    IO.delay(
      VSWorkspace.workspaceFolders.toOption
        .map(_.toVector)
        .getOrElse(Vector.empty),
    ).map(WorkspaceFolderLogic.parse)
      .flatMap: parsed =>
        parsed.problems.toList
          .traverse_(problem =>
            output
              .append(s"Skipped ${problem.message}\n")
              .handleError(_ => ()),
          )
          .as(WorkspaceFolderLogic.decide(parsed))
      .flatMap:
        case WorkspaceSelectionDecision.NoFolders =>
          output
            .report("An open workspace folder is required.")
            .handleError(_ => ())
            .as(None)
        case WorkspaceSelectionDecision.NoValidFolders =>
          output
            .report("None of the open workspace folders could be used.")
            .handleError(_ => ())
            .as(None)
        case WorkspaceSelectionDecision.Selected(workspace) =>
          IO.pure(Some(workspace))
        case WorkspaceSelectionDecision.Pick(workspaces) =>
          val candidates       = WorkspaceOptions.from(workspaces)
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

private[infrastructure] object WorkspaceFolderLogic:
  def parse(folders: Vector[WorkspaceFolder]): ParsedWorkspaces =
    val (problems, workspaces) = folders.partitionMap(fromFolder)
    ParsedWorkspaces(
      problems = problems,
      workspaces = workspaces,
    )

  def decide(parsed: ParsedWorkspaces): WorkspaceSelectionDecision =
    val folderCount = parsed.problems.size + parsed.workspaces.size
    (folderCount, parsed.workspaces) match
      case (0, _)               => WorkspaceSelectionDecision.NoFolders
      case (_, Vector())        => WorkspaceSelectionDecision.NoValidFolders
      case (1, Vector(current)) =>
        WorkspaceSelectionDecision.Selected(current)
      case (_, candidates) => WorkspaceSelectionDecision.Pick(candidates)

  def fromFolder(
      folder: WorkspaceFolder,
  ): Either[ConfigProblem, Workspace] =
    val uri = folder.uri.toString()
    WorkspaceId
      .parse(uri)
      .leftMap:
        case ConfigProblem.InvalidValue(_, detail) =>
          ConfigProblem.InvalidValue(
            s"workspace folder '${folder.name}' URI",
            detail,
          )
        case problem => problem
      .map: key =>
        Workspace(
          key = key,
          name = folder.name,
          root = folder.uri.fsPath,
        )

private[infrastructure] final case class ParsedWorkspaces(
    problems: Vector[ConfigProblem],
    workspaces: Vector[Workspace],
)

private[infrastructure] enum WorkspaceSelectionDecision:
  case NoFolders, NoValidFolders
  case Selected(workspace: Workspace)
  case Pick(workspaces: Vector[Workspace])

private[infrastructure] object WorkspaceCleanup:
  def stopRemoved(
      folders: List[WorkspaceFolder],
      stop: Workspace => IO[Unit],
      onError: Throwable => IO[Unit],
  ): IO[Unit] =
    folders.traverse_(folder =>
      IO.delay(WorkspaceFolderLogic.fromFolder(folder))
        .flatMap(parsed =>
          IO.fromEither(
            parsed.leftMap(problem => IllegalArgumentException(problem.message)),
          ),
        )
        .flatMap(stop)
        .handleErrorWith(error => onError(error).handleError(_ => ())),
    )

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
          trusted(requireTrust) *> WorkspaceSelection
            .choose(output)
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
      val stop = WorkspaceCleanup.stopRemoved(
        folders = event.removed.toList,
        stop = service.stopSilently,
        onError = error => log(output, error),
      )
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
      .report(ThrowableMessage.from(error))
      .handleError(_ => ())

  private def log(output: VscodeOutput, error: Throwable): IO[Unit] =
    output
      .append(s"ERROR: ${ThrowableMessage.from(error)}\n")
      .handleError(_ => ())
