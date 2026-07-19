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
    case UnsupportedVersion(actual) => s"version must be 1, but was $actual"
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

opaque type WorkingDirectory = String

object WorkingDirectory:
  def parseResolved(
      resolved: String,
      relativeFromRoot: String,
      separator: String,
      relativeIsAbsolute: Boolean,
  ): Either[ConfigProblem, WorkingDirectory] =
    val escapes =
      relativeFromRoot == ".." ||
        relativeFromRoot.startsWith(".." + separator) ||
        relativeIsAbsolute
    Either.cond(
      resolved.nonEmpty && !escapes,
      resolved,
      ConfigProblem.InvalidValue(
        "preview.workingDirectory",
        "must resolve inside the workspace folder",
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
    url: PreviewUrl,
    startupTimeout: StartupTimeout,
):
  def resolve(workingDirectory: WorkingDirectory): PreviewCommand =
    PreviewCommand(executable, arguments, workingDirectory, url, startupTimeout)

final case class PreviewCommand(
    executable: Executable,
    arguments: Vector[ProcessArgument],
    workingDirectory: WorkingDirectory,
    url: PreviewUrl,
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

final case class Workspace(key: String, name: String, root: String, uri: String)

final case class ProcessExit(
    code: Option[Int],
    signal: Option[String],
    error: Option[String],
):
  def render: String =
    error
      .orElse(code.map(value => s"exit code $value"))
      .orElse(signal.map(value => s"signal $value"))
      .getOrElse("an unknown status")

enum PreviewViewState:
  case Starting
  case Ready(url: PreviewUrl)
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
  case Starting, Ready, Stopping
  case Terminal(value: PreviewTerminal)
