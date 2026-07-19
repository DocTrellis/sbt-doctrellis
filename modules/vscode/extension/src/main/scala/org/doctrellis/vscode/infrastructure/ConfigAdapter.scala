package org.doctrellis.vscode.infrastructure

import cats.effect.IO
import cats.syntax.all.*
import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.{NodeFsPromises, NodePath}

import scala.scalajs.js

private[infrastructure] object ConfigParser:
  def parse(
      text: String,
      isAbsolute: String => Boolean,
  ): Either[ConfigProblem, PreviewTemplate] =
    decodeJson(text).flatMap: raw =>
      for
        root    <- objectAt(raw, "$", Set("version", "preview"))
        version <- integerAt(root, "version", "version")
        _       <- Either.cond(
          version == 1 || version == 2,
          (),
          ConfigProblem.UnsupportedVersion(version),
        )
        previewRaw   <- fieldAt(root, "preview", "preview")
        preview      <- previewObject(previewRaw, version)
        commandRaw   <- stringAt(preview, "command", "preview.command")
        command      <- Executable.parse(commandRaw)
        argumentText <- stringArrayAt(preview, "args", "preview.args")
        arguments <- argumentText.zipWithIndex.traverse(ProcessArgument.parse)
        directoryRaw <- stringAt(
          preview,
          "workingDirectory",
          "preview.workingDirectory",
        )
        directory  <- RelativeDirectory.parse(directoryRaw, isAbsolute)
        endpoint   <- previewEndpoint(preview, version)
        timeoutRaw <- integerAt(
          preview,
          "startupTimeoutMs",
          "preview.startupTimeoutMs",
        )
        timeout <- StartupTimeout.parse(timeoutRaw)
      yield PreviewTemplate(
        executable = command,
        arguments = arguments,
        relativeDirectory = directory,
        endpoint = endpoint,
        startupTimeout = timeout,
      )

  private def previewObject(
      raw: js.Any,
      version: Int,
  ): Either[ConfigProblem, js.Dynamic] =
    val common = Set("command", "args", "workingDirectory", "startupTimeoutMs")
    val fields = if version == 1 then common + "url" else common + "endpoint"
    objectAt(raw, "preview", fields)

  private def previewEndpoint(
      preview: js.Dynamic,
      version: Int,
  ): Either[ConfigProblem, PreviewEndpoint] =
    if version == 1 then
      for
        raw <- stringAt(preview, "url", "preview.url")
        url <- PreviewUrl.parse(raw)
      yield PreviewEndpoint.Fixed(url)
    else
      for
        raw      <- fieldAt(preview, "endpoint", "preview.endpoint")
        endpoint <- objectAt(
          raw,
          "preview.endpoint",
          Set("source", "protocol"),
        )
        source <- stringAt(endpoint, "source", "preview.endpoint.source")
        _      <- exact(
          actual = source,
          expected = "ipc",
          path = "preview.endpoint.source",
        )
        protocol <- stringAt(
          endpoint,
          "protocol",
          "preview.endpoint.protocol",
        )
        _ <- exact(
          actual = protocol,
          expected = "doctrellis-preview-v1",
          path = "preview.endpoint.protocol",
        )
      yield PreviewEndpoint.Announced

  private def exact(
      actual: String,
      expected: String,
      path: String,
  ): Either[ConfigProblem, Unit] =
    Either.cond(
      actual == expected,
      (),
      ConfigProblem.InvalidValue(path, s"must be '$expected'"),
    )

  private def decodeJson(text: String): Either[ConfigProblem, js.Any] =
    try Right(js.JSON.parse(text))
    catch
      case error: js.JavaScriptException =>
        Left(ConfigProblem.InvalidJson(error.getMessage))

  private def objectAt(
      value: js.Any,
      path: String,
      allowedFields: Set[String],
  ): Either[ConfigProblem, js.Dynamic] =
    if value == null || js.typeOf(value) != "object" || js.Array.isArray(value)
    then Left(ConfigProblem.Expected(path, "an object"))
    else
      val dynamic = value.asInstanceOf[js.Dynamic]
      val actual  = js.Object.keys(value.asInstanceOf[js.Object]).iterator.toSet
      actual.diff(allowedFields).headOption match
        case Some(field) =>
          Left(ConfigProblem.Unexpected(qualified(path, field)))
        case None => Right(dynamic)

  private def fieldAt(
      value: js.Dynamic,
      name: String,
      path: String,
  ): Either[ConfigProblem, js.Any] =
    val field = value.selectDynamic(name)
    Either.cond(!js.isUndefined(field), field, ConfigProblem.Missing(path))

  private def stringAt(
      value: js.Dynamic,
      name: String,
      path: String,
  ): Either[ConfigProblem, String] =
    fieldAt(value, name, path).flatMap: field =>
      Either.cond(
        js.typeOf(field) == "string",
        field.asInstanceOf[String],
        ConfigProblem.Expected(path, "a string"),
      )

  private def integerAt(
      value: js.Dynamic,
      name: String,
      path: String,
  ): Either[ConfigProblem, Int] =
    fieldAt(value, name, path).flatMap: field =>
      val valid = js.typeOf(field) == "number" &&
        field.asInstanceOf[Double].isWhole &&
        field.asInstanceOf[Double] >= Int.MinValue &&
        field.asInstanceOf[Double] <= Int.MaxValue
      Either.cond(
        valid,
        field.asInstanceOf[Double].toInt,
        ConfigProblem.Expected(path, "an integer"),
      )

  private def stringArrayAt(
      value: js.Dynamic,
      name: String,
      path: String,
  ): Either[ConfigProblem, Vector[String]] =
    fieldAt(value, name, path).flatMap: field =>
      if !js.Array.isArray(field) then
        Left(ConfigProblem.Expected(path, "an array of strings"))
      else
        val values = field.asInstanceOf[js.Array[js.Any]]
        Either.cond(
          values.forall(js.typeOf(_) == "string"),
          values.iterator.map(_.asInstanceOf[String]).toVector,
          ConfigProblem.Expected(path, "an array containing only strings"),
        )

  private def qualified(parent: String, child: String): String =
    if parent == "$" then child else s"$parent.$child"

private[infrastructure] object NodeConfig:
  def load(workspace: Workspace): IO[PreviewCommand] =
    for
      text <- promise(
        NodeFsPromises
          .readFile(NodePath.join(workspace.root, ".doctrellis.json"), "utf8"),
        s"could not read ${workspace.root}/.doctrellis.json",
      )
      template <- IO.fromEither(
        ConfigParser
          .parse(text, NodePath.isAbsolute)
          .leftMap(problem => IllegalArgumentException(problem.message)),
      )
      root <- promise(
        NodeFsPromises.realpath(workspace.root),
        "could not resolve the workspace folder",
      )
      candidate <- promise(
        NodeFsPromises.realpath(
          NodePath.resolve(root, template.relativeDirectory.value),
        ),
        s"could not resolve preview.workingDirectory ${template.relativeDirectory.value}",
      )
      relative = NodePath.relative(root, candidate)
      directory <- IO.fromEither(
        WorkingDirectory
          .parseResolved(
            candidate,
            relative,
            NodePath.sep,
            NodePath.isAbsolute(relative),
          )
          .leftMap(problem => IllegalArgumentException(problem.message)),
      )
    yield template.resolve(directory)

  private def promise[A](value: => js.Promise[A], context: String): IO[A] =
    IO.fromPromise(IO.delay(value))
      .adaptError: error =>
        IllegalArgumentException(s"$context: ${error.getMessage}", error)
