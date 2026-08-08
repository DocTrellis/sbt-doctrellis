package org.doctrellis.vscode.domain

import scala.concurrent.duration.*

enum ConfigProblem:
  case InvalidJson(detail: String)
  case Expected(path: String, expected: String)
  case Missing(path: String)
  case Unexpected(path: String)
  case UnsupportedVersion(actual: Int)
  case InvalidValue(path: String, detail: String)

  def message: String = this match
    case InvalidJson(detail)        => s"invalid JSON: $detail"
    case Expected(path, expected)   => s"$path must be $expected"
    case Missing(path)              => s"missing required field: $path"
    case Unexpected(path)           => s"unexpected field: $path"
    case UnsupportedVersion(actual) =>
      s"version must be 1 or 2, but was $actual"
    case InvalidValue(path, detail) => s"$path $detail"

opaque type Executable = String

object Executable:
  def parse(value: String): Either[ConfigProblem, Executable] =
    Either.cond(
      value.trim.nonEmpty && !value.contains('\u0000'),
      value,
      ConfigProblem.InvalidValue(
        "preview.command",
        "must be a non-empty executable name",
      ),
    )

  extension (value: Executable) def value: String = value

opaque type ProcessArgument = String

object ProcessArgument:
  def parse(value: String, index: Int): Either[ConfigProblem, ProcessArgument] =
    Either.cond(
      !value.contains('\u0000'),
      value,
      ConfigProblem.InvalidValue(
        s"preview.args[$index]",
        "must not contain a NUL character",
      ),
    )

  extension (value: ProcessArgument) def value: String = value

opaque type RelativeDirectory = String

object RelativeDirectory:
  def parse(
      value: String,
      isAbsolute: String => Boolean,
  ): Either[ConfigProblem, RelativeDirectory] =
    Either.cond(
      value.trim.nonEmpty && !value.contains('\u0000') && !isAbsolute(value),
      value,
      ConfigProblem.InvalidValue(
        "preview.workingDirectory",
        "must be a non-empty path relative to the workspace folder",
      ),
    )

  extension (value: RelativeDirectory) def value: String = value

final class PathSemantics(
    val separator: String,
    val isAbsolute: String => Boolean,
)

opaque type WorkingDirectory = String

object WorkingDirectory:
  def parseResolved(
      resolved: String,
      relativeFromRoot: String,
      paths: PathSemantics,
  ): Either[ConfigProblem, WorkingDirectory] =
    val escapes =
      relativeFromRoot == ".." ||
        relativeFromRoot.startsWith(".." + paths.separator) ||
        paths.isAbsolute(relativeFromRoot)
    Either.cond(
      resolved.nonEmpty && paths.isAbsolute(resolved) && !escapes,
      resolved,
      ConfigProblem.InvalidValue(
        "preview.workingDirectory",
        "must resolve to an absolute path inside the workspace folder",
      ),
    )

  extension (value: WorkingDirectory) def value: String = value

opaque type PreviewUrl = String

object PreviewUrl:
  private val HttpUrl =
    """https?://(\[[0-9A-Fa-f:.]+\]|[A-Za-z0-9](?:[A-Za-z0-9.-]*[A-Za-z0-9])?)(?::([0-9]{1,5}))?([/?#][^\s]*)?""".r

  def parse(value: String): Either[ConfigProblem, PreviewUrl] =
    val valid = value match
      case HttpUrl(_, port, _) =>
        Option(port).forall(
          _.toIntOption.exists(value => value > 0 && value <= 65535),
        )
      case _ => false
    Either.cond(
      valid,
      value,
      ConfigProblem.InvalidValue(
        "preview.url",
        "must be a well-formed absolute http:// or https:// URL",
      ),
    )

  extension (value: PreviewUrl) def value: String = value

enum SiteRevisionProblem:
  case NonPositive(actual: Long)

  def message: String = this match
    case NonPositive(actual) =>
      s"site revision must be positive, but was $actual"

opaque type SiteRevision = Long

object SiteRevision:
  def parse(value: Long): Either[SiteRevisionProblem, SiteRevision] =
    Either.cond(value > 0L, value, SiteRevisionProblem.NonPositive(value))

  extension (value: SiteRevision)
    def value: Long = value

    def isAfter(previous: SiteRevision): Boolean = value > previous

opaque type StartupTimeout = FiniteDuration

object StartupTimeout:
  def parse(milliseconds: Int): Either[ConfigProblem, StartupTimeout] =
    Either.cond(
      milliseconds > 0 && milliseconds <= 600000,
      milliseconds.millis,
      ConfigProblem.InvalidValue(
        "preview.startupTimeoutMs",
        "must be between 1 and 600000",
      ),
    )

  extension (value: StartupTimeout) def duration: FiniteDuration = value

opaque type PollInterval = FiniteDuration

object PollInterval:
  def parse(milliseconds: Int): Either[ConfigProblem, PollInterval] =
    Either.cond(
      milliseconds >= 100 && milliseconds <= 10000,
      milliseconds.millis,
      ConfigProblem.InvalidValue(
        "doctrellis.pollIntervalMs",
        "must be between 100 and 10000",
      ),
    )

  extension (value: PollInterval) def duration: FiniteDuration = value

final case class PreviewTemplate(
    executable: Executable,
    arguments: Vector[ProcessArgument],
    relativeDirectory: RelativeDirectory,
    endpoint: PreviewEndpoint,
    startupTimeout: StartupTimeout,
):
  def resolve(workingDirectory: WorkingDirectory): PreviewCommand =
    PreviewCommand(
      executable = executable,
      arguments = arguments,
      workingDirectory = workingDirectory,
      endpoint = endpoint,
      startupTimeout = startupTimeout,
    )

enum PreviewEndpoint:
  case Fixed(url: PreviewUrl)
  case Announced

  def description: String = this match
    case Fixed(url) => url.value
    case Announced  => "the workspace IPC endpoint"

final case class PreviewCommand(
    executable: Executable,
    arguments: Vector[ProcessArgument],
    workingDirectory: WorkingDirectory,
    endpoint: PreviewEndpoint,
    startupTimeout: StartupTimeout,
)

final case class PreviewPolicy(
    openWhenReady: Boolean,
    pollInterval: PollInterval,
)

object PreviewPolicy:
  def parse(
      openWhenReady: Boolean,
      pollIntervalMs: Int,
  ): Either[ConfigProblem, PreviewPolicy] =
    PollInterval.parse(pollIntervalMs).map(PreviewPolicy(openWhenReady, _))

opaque type WorkspaceId = String

object WorkspaceId:
  private val UriWithScheme = """[A-Za-z][A-Za-z0-9+.-]*:.*""".r

  def parse(value: String): Either[ConfigProblem, WorkspaceId] =
    val hasScheme = value match
      case UriWithScheme() => true
      case _               => false
    val hasInvalidCharacter = value.exists(character =>
      Character.isISOControl(character) || character.isWhitespace ||
        character.isSpaceChar,
    )
    Either.cond(
      hasScheme && !hasInvalidCharacter,
      value,
      ConfigProblem.InvalidValue(
        "workspace.key",
        "must have a URI scheme and contain neither whitespace nor control characters",
      ),
    )

  extension (value: WorkspaceId) def value: String = value

final case class Workspace(
    key: WorkspaceId,
    name: String,
    root: String,
)

enum ProcessTermination:
  case Code(value: Int)
  case Signal(value: String)
  case Ambiguous(code: Int, signal: String)

  def render: String = this match
    case Code(value)             => s"exit code $value"
    case Signal(value)           => s"signal $value"
    case Ambiguous(code, signal) =>
      s"exit code $code and signal $signal"

enum ProcessExit:
  case Terminated(termination: ProcessTermination)
  case Failed(reason: String, termination: Option[ProcessTermination])
  case Unknown

  def render: String = this match
    case Terminated(termination)           => termination.render
    case Failed(reason, Some(termination)) =>
      s"$reason (${termination.render})"
    case Failed(reason, None) => reason
    case Unknown              => "an unknown status"

object ProcessExit:
  def fromObservation(
      code: Option[Int],
      signal: Option[String],
      error: Option[String],
  ): ProcessExit =
    val termination = (code, signal) match
      case (Some(value), None) => Some(ProcessTermination.Code(value))
      case (None, Some(value)) => Some(ProcessTermination.Signal(value))
      case (Some(exitCode), Some(exitSignal)) =>
        Some(ProcessTermination.Ambiguous(exitCode, exitSignal))
      case (None, None) => None

    (error, termination) match
      case (Some(reason), observed) => ProcessExit.Failed(reason, observed)
      case (None, Some(observed))   => ProcessExit.Terminated(observed)
      case (None, None)             => ProcessExit.Unknown

enum PreviewViewState:
  case Starting
  case Ready(url: PreviewUrl, supportsLiveRefresh: Boolean)
  case Refreshed(url: PreviewUrl, revision: SiteRevision)
  case Stopped
  case Failed(reason: String)
  case Exited(exit: ProcessExit)

enum PreviewTerminal:
  case Failed(reason: String)
  case Exited(exit: ProcessExit)

  def viewState: PreviewViewState = this match
    case Failed(reason) => PreviewViewState.Failed(reason)
    case Exited(exit)   => PreviewViewState.Exited(exit)

enum PreviewStatus:
  case Starting, Stopping
  case Ready(url: PreviewUrl, revision: Option[SiteRevision])
  case Terminal(value: PreviewTerminal)

enum ReadyAcceptance:
  case Accepted, Rejected

enum TerminalAcceptance:
  case Accepted, Rejected

object PreviewStatusTransition:
  def ready(
      current: PreviewStatus,
      url: PreviewUrl,
  ): (PreviewStatus, ReadyAcceptance) =
    current match
      case PreviewStatus.Starting =>
        PreviewStatus.Ready(url, None) -> ReadyAcceptance.Accepted
      case current @ PreviewStatus.Ready(_, _) =>
        current -> ReadyAcceptance.Accepted
      case PreviewStatus.Stopping =>
        PreviewStatus.Stopping -> ReadyAcceptance.Rejected
      case current @ PreviewStatus.Terminal(_) =>
        current -> ReadyAcceptance.Rejected

  def refresh(
      current: PreviewStatus,
      revision: SiteRevision,
  ): (PreviewStatus, Option[PreviewStatus]) =
    current match
      case PreviewStatus.Ready(url, _) =>
        val updated = PreviewStatus.Ready(url, Some(revision))
        updated -> Some(updated)
      case current @ (PreviewStatus.Starting | PreviewStatus.Stopping |
          PreviewStatus.Terminal(_)) =>
        current -> None

  def stopping(current: PreviewStatus): PreviewStatus =
    current match
      case PreviewStatus.Starting | PreviewStatus.Stopping |
          PreviewStatus.Ready(_, _) | PreviewStatus.Terminal(_) =>
        PreviewStatus.Stopping

  def terminal(
      current: PreviewStatus,
      terminal: PreviewTerminal,
  ): (PreviewStatus, TerminalAcceptance) =
    current match
      case PreviewStatus.Stopping =>
        PreviewStatus.Stopping -> TerminalAcceptance.Rejected
      case PreviewStatus.Starting | PreviewStatus.Ready(_, _) |
          PreviewStatus.Terminal(_) =>
        PreviewStatus.Terminal(terminal) -> TerminalAcceptance.Accepted
