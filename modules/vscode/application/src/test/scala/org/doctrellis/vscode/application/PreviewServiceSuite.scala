package org.doctrellis.vscode.application

import cats.effect.{Deferred, IO, Ref, Resource}
import cats.syntax.all.*
import munit.CatsEffectSuite
import org.doctrellis.vscode.domain.*

class PreviewServiceSuite extends CatsEffectSuite:
  private val workspace =
    Workspace("file:///workspace", "example", "/workspace", "file:///workspace")

  private val command =
    PreviewCommand(
      right(Executable.parse("sbt")),
      Vector(right(ProcessArgument.parse("doctrellisPreview", 0))),
      right(WorkingDirectory.parseResolved("/workspace", "", "/", false)),
      right(PreviewUrl.parse("http://127.0.0.1:4242")),
      right(StartupTimeout.parse(90000)),
    )

  private val policy = PreviewPolicy(false, right(PollInterval.parse(500)))

  test("starts at most one process for a workspace"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      ports = healthyPorts(launches, releases)
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          service.start(workspace) *> service.start(workspace) *> launches.get
            .map(assertEquals(_, 1))
      released <- releases.get
    yield assertEquals(released, 1)

  test("serializes concurrent starts for the same workspace"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      ports = healthyPorts(launches, releases)
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          List.fill(8)(service.start(workspace)).parSequence_ *> launches.get
            .map(assertEquals(_, 1))
      released <- releases.get
    yield assertEquals(released, 1)

  test("reports an exit that happens before readiness"):
    val exit  = ProcessExit(Some(1), None, None)
    val ports = PreviewPorts(
      _ => IO.pure(command),
      _ => IO.pure(policy),
      (_, _) => Resource.pure(RunningProcess(IO.pure(exit))),
      _ => IO.never,
      _ => IO.unit,
      _ => IO.unit,
      _ => IO.unit,
    )
    PreviewService
      .resource(ports)
      .use: service =>
        service
          .start(workspace)
          .attempt
          .map: result =>
            assert(result.swap.exists(_.isInstanceOf[PreviewExited]))

  test("stop releases the running process"):
    for
      launches <- Ref.of[IO, Int](0)
      releases <- Ref.of[IO, Int](0)
      ports = healthyPorts(launches, releases)
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          service.start(workspace) *> service.stop(workspace)
      released <- releases.get
    yield assertEquals(released, 1)

  test("stop settles a start that is still waiting for readiness"):
    for
      launched <- Deferred[IO, Unit]
      released <- Ref.of[IO, Int](0)
      ports = PreviewPorts(
        _ => IO.pure(command),
        _ => IO.pure(policy),
        (_, _) =>
          Resource.make(
            launched.complete(()).as(RunningProcess(IO.never)),
          )(_ => released.update(_ + 1)),
        _ => IO.never,
        _ => IO.unit,
        _ => IO.unit,
        _ => IO.unit,
      )
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          for
            start <- service.start(workspace).start
            _     <- launched.get
            _     <- service.stop(workspace)
            _     <- start.joinWithNever
          yield ()
      count <- released.get
    yield assertEquals(count, 1)

  test("propagates an await-exit failure during startup"):
    val failure = IllegalStateException("child process observation failed")
    val ports   = PreviewPorts(
      _ => IO.pure(command),
      _ => IO.pure(policy),
      (_, _) => Resource.pure(RunningProcess(IO.raiseError(failure))),
      _ => IO.never,
      _ => IO.unit,
      _ => IO.unit,
      _ => IO.unit,
    )
    PreviewService
      .resource(ports)
      .use(_.start(workspace).attempt)
      .map(assertEquals(_, Left(failure)))

  test("attempts every release when one session cleanup fails"):
    val second = workspace.copy(
      key = "file:///workspace-two",
      name = "example-two",
      root = "/workspace-two",
      uri = "file:///workspace-two",
    )
    for
      releases <- Ref.of[IO, Int](0)
      ports = PreviewPorts(
        _ => IO.pure(command),
        _ => IO.pure(policy),
        (current, _) =>
          Resource.make(IO.pure(RunningProcess(IO.never)))(_ =>
            releases.update(_ + 1) *>
              IO.raiseWhen(current.key == workspace.key)(
                IllegalStateException("release failed"),
              ),
          ),
        _ => IO.pure(true),
        _ => IO.unit,
        _ => IO.unit,
        _ => IO.unit,
      )
      _ <- PreviewService
        .resource(ports)
        .use: service =>
          service.start(workspace) *>
            service.start(second) *>
            service.stopAll
      count <- releases.get
    yield assertEquals(count, 2)

  private def healthyPorts(
      launches: Ref[IO, Int],
      releases: Ref[IO, Int],
  ): PreviewPorts =
    PreviewPorts(
      _ => IO.pure(command),
      _ => IO.pure(policy),
      (_, _) =>
        Resource.make(launches.update(_ + 1).as(RunningProcess(IO.never)))(_ =>
          releases.update(_ + 1),
        ),
      _ => IO.pure(true),
      _ => IO.unit,
      _ => IO.unit,
      _ => IO.unit,
    )

  private def right[A](value: Either[ConfigProblem, A]): A =
    value.fold(problem => fail(problem.message), identity)
