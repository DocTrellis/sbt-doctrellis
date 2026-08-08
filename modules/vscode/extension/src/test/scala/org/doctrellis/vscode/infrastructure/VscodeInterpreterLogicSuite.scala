package org.doctrellis.vscode.infrastructure

import scala.concurrent.duration.*
import scala.scalajs.js

import cats.effect.{Deferred, IO, Ref}

import munit.CatsEffectSuite

import org.doctrellis.vscode.domain.*
import org.doctrellis.vscode.infrastructure.facade.{
  OutputChannel,
  WorkspaceFolder,
}

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

  test("error reporting does not await the user notification"):
    val events = js.Array[String]()
    for
      notificationStarted  <- Deferred[IO, Unit]
      keepNotificationOpen <- Deferred[IO, Unit]
      notifications        <- Ref.of[IO, Vector[String]](Vector.empty)
      _                    <- BackgroundTasks.resource.use: tasks =>
        val output = VscodeOutput(
          channel = outputChannel(events),
          backgroundTasks = tasks,
          notifyInformation = _ => IO.unit,
          notifyError = message =>
            notifications.update(_ :+ message) *>
              notificationStarted.complete(()).void *>
              keepNotificationOpen.get,
        )
        output.report("failure").timeout(1.second) *>
          notificationStarted.get.timeout(1.second)
      actualNotifications <- notifications.get
    yield
      assertEquals(
        events.toVector,
        Vector("append:ERROR: failure\n", "show:true"),
      )
      assertEquals(actualNotifications, Vector("DocTrellis: failure"))

  test("information reporting writes and sends an unprefixed notification"):
    val events = js.Array[String]()
    for
      notificationStarted <- Deferred[IO, Unit]
      notifications       <- Ref.of[IO, Vector[String]](Vector.empty)
      _                   <- BackgroundTasks.resource.use: tasks =>
        val output = VscodeOutput(
          channel = outputChannel(events),
          backgroundTasks = tasks,
          notifyInformation = message =>
            notifications.update(_ :+ message) *>
              notificationStarted.complete(()).void,
          notifyError = _ => IO.unit,
        )
        output.info("ready") *>
          notificationStarted.get.timeout(1.second)
      actualNotifications <- notifications.get
    yield
      assertEquals(events.toVector, Vector("append:ready\n"))
      assertEquals(actualNotifications, Vector("ready"))

  test("multi-root selection keeps labels and workspaces independent"):
    val first  = workspace("/work/first", "file:///work/first")
    val second = workspace("/work/second", "file:///work/second")
    val actual = WorkspaceOptions.from(Vector(first, second))

    assertEquals(
      actual.map(_._1),
      Vector("docs — /work/first", "docs — /work/second"),
    )
    assertEquals(
      actual.map(_._2.key.value),
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

  test("workspace selection keeps valid folders when another URI is malformed"):
    val valid    = folder("first", "/work/first", "file:///work/first")
    val invalid  = folder("invalid", "/work/invalid", "invalid\u0000uri")
    val parsed   = WorkspaceFolderLogic.parse(Vector(valid, invalid))
    val rejected = WorkspaceFolderLogic.parse(Vector(invalid))
    val selected = WorkspaceFolderLogic.parse(Vector(valid))
    val empty    = WorkspaceFolderLogic.parse(Vector.empty)

    assertEquals(parsed.problems.size + parsed.workspaces.size, 2)
    assertEquals(parsed.problems.size, 1)
    assert(parsed.problems.exists(_.message.contains("invalid")))
    assertEquals(
      parsed.workspaces.map(_.key.value),
      Vector("file:///work/first"),
    )
    assertEquals(
      WorkspaceFolderLogic.decide(parsed),
      WorkspaceSelectionDecision.Pick(parsed.workspaces),
    )
    assertEquals(
      WorkspaceFolderLogic.decide(rejected),
      WorkspaceSelectionDecision.NoValidFolders,
    )
    assertEquals(
      WorkspaceFolderLogic.decide(selected),
      WorkspaceSelectionDecision.Selected(
        workspace("/work/first", "file:///work/first").copy(name = "first"),
      ),
    )
    assertEquals(
      WorkspaceFolderLogic.decide(empty),
      WorkspaceSelectionDecision.NoFolders,
    )

  test("removed workspace cleanup attempts every folder after a failure"):
    val invalid = folder("invalid", "/work/invalid", "invalid\u0000uri")
    val first   = folder("first", "/work/first", "file:///work/first")
    val second  = folder("second", "/work/second", "file:///work/second")
    val firstId = WorkspaceId
      .parse("file:///work/first")
      .fold(problem => fail(problem.message), identity)

    for
      stopped  <- Ref.of[IO, Vector[WorkspaceId]](Vector.empty)
      reported <- Ref.of[IO, Vector[String]](Vector.empty)
      _        <- WorkspaceCleanup.stopRemoved(
        folders = List(invalid, first, second),
        stop = workspace =>
          stopped.update(_ :+ workspace.key) *>
            IO.raiseWhen(workspace.key == firstId)(
              IllegalStateException("first cleanup failed"),
            ),
        onError = error => reported.update(_ :+ error.getMessage),
      )
      actualStops   <- stopped.get
      actualReports <- reported.get
    yield
      assertEquals(
        actualStops.map(_.value),
        Vector("file:///work/first", "file:///work/second"),
      )
      assertEquals(actualReports.size, 2)
      assert(actualReports.exists(_.contains("invalid")))
      assert(actualReports.contains("first cleanup failed"))

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
    Workspace(
      key = WorkspaceId
        .parse(value)
        .fold(problem => fail(problem.message), identity),
      name = "docs",
      root = root,
    )

  private def folder(
      name: String,
      root: String,
      value: String,
  ): WorkspaceFolder =
    val uri = js.Dynamic.literal(fsPath = root)
    uri.updateDynamic("toString")((() => value): js.Function0[String])
    js.Dynamic
      .literal(name = name, uri = uri)
      .asInstanceOf[WorkspaceFolder]

  private def outputChannel(events: js.Array[String]): OutputChannel =
    val append: js.Function1[String, Unit] = value =>
      events.push(s"append:$value"): Unit
    val show: js.Function1[Boolean, Unit] = preserveFocus =>
      events.push(s"show:$preserveFocus"): Unit
    val dispose: js.Function0[Unit] = () => ()
    js.Dynamic
      .literal(append = append, show = show, dispose = dispose)
      .asInstanceOf[OutputChannel]
