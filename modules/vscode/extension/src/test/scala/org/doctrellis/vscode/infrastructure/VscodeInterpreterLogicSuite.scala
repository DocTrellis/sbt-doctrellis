package org.doctrellis.vscode.infrastructure

import scala.concurrent.duration.*
import scala.scalajs.js

import cats.effect.{Deferred, IO}

import munit.CatsEffectSuite

import org.doctrellis.vscode.domain.*

class VscodeInterpreterLogicSuite extends CatsEffectSuite:
  test("cancels owned background tasks when their resource closes"):
    for
      started  <- Deferred[IO, Unit]
      canceled <- Deferred[IO, Unit]
      _        <- BackgroundTasks.resource.use: tasks =>
        tasks.submit(
          (started.complete(()) *> IO.never[Unit])
            .onCancel(canceled.complete(()).void),
        ) *> started.get
      _ <- canceled.get.timeout(1.second)
    yield ()

  test("multi-root selection keeps labels and workspaces independent"):
    val first  = workspace("/work/first", "file:///work/first")
    val second = workspace("/work/second", "file:///work/second")
    val actual = WorkspaceOptions.from(Vector(first, second))

    assertEquals(
      actual.map(_._1),
      Vector("docs — /work/first", "docs — /work/second"),
    )
    assertEquals(
      actual.map(_._2.key),
      Vector("file:///work/first", "file:///work/second"),
    )

  test("workspace selection resolves known labels without partial indexing"):
    val first      = workspace("/work/first", "file:///work/first")
    val second     = workspace("/work/second", "file:///work/second")
    val candidates = WorkspaceOptions.from(Vector(first, second))

    assertEquals(
      WorkspaceOptions.select(candidates, "docs — /work/second"),
      Some(second),
    )
    assertEquals(WorkspaceOptions.select(candidates, "unknown"), None)

  test("VS Code preferences parse typed boundary values"):
    val parsed = VscodePreferencesParser.parse(
      openWhenReady = true,
      pollIntervalMs = 500,
    )

    assertEquals(
      parsed.map(policy =>
        policy.openWhenReady -> policy.pollInterval.duration,
      ),
      Right(true -> 500.millis),
    )

  test("VS Code preferences reject malformed JavaScript types"):
    val boolean = VscodePreferencesParser.parse(
      openWhenReady = "true",
      pollIntervalMs = 500,
    )
    val stringInterval = VscodePreferencesParser.parse(
      openWhenReady = true,
      pollIntervalMs = "500",
    )
    val fractionalInterval = VscodePreferencesParser.parse(
      openWhenReady = true,
      pollIntervalMs = 500.5,
    )
    val infiniteInterval = VscodePreferencesParser.parse(
      openWhenReady = true,
      pollIntervalMs = Double.PositiveInfinity,
    )
    val objectInterval = VscodePreferencesParser.parse(
      openWhenReady = true,
      pollIntervalMs = js.Dynamic.literal(value = 500),
    )

    assertEquals(
      boolean,
      Left(
        ConfigProblem.Expected(
          "doctrellis.openPreviewWhenReady",
          "a boolean",
        ),
      ),
    )
    List(stringInterval, fractionalInterval, infiniteInterval, objectInterval)
      .foreach(result =>
        assertEquals(
          result,
          Left(
            ConfigProblem.Expected(
              "doctrellis.pollIntervalMs",
              "an integer",
            ),
          ),
        ),
      )

  test("VS Code preferences preserve the poll interval domain range"):
    val below = VscodePreferencesParser.parse(
      openWhenReady = true,
      pollIntervalMs = 99,
    )
    val above = VscodePreferencesParser.parse(
      openWhenReady = true,
      pollIntervalMs = 10001,
    )

    List(below, above).foreach(result =>
      assertEquals(
        result,
        Left(
          ConfigProblem.InvalidValue(
            "doctrellis.pollIntervalMs",
            "must be between 100 and 10000",
          ),
        ),
      ),
    )

  test("trust validation gates only commands that require trust"):
    val required = WorkspaceTrust.validate(required = true, trusted = false)
    val cleanup  = WorkspaceTrust.validate(required = false, trusted = false)
    val trusted  = WorkspaceTrust.validate(required = true, trusted = true)

    assert(required.isLeft)
    assertEquals(cleanup, Right(()))
    assertEquals(trusted, Right(()))

  private def workspace(root: String, value: String): Workspace =
    Workspace(value, "docs", root, value)
