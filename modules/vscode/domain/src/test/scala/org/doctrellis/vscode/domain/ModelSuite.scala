package org.doctrellis.vscode.domain

class ModelSuite extends munit.FunSuite:
  private val posixPaths = new PathSemantics("/", _.startsWith("/"))

  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  test("parses values into types that retain their invariants"):
    assertEquals(Executable.parse("sbt").map(_.value), Right("sbt"))
    assertEquals(
      PreviewUrl.parse("http://127.0.0.1:4242").map(_.value),
      Right("http://127.0.0.1:4242"),
    )
    assertEquals(
      StartupTimeout.parse(90000).map(_.duration.toMillis),
      Right(90000L),
    )

  test("rejects values that cannot inhabit the domain types"):
    assert(Executable.parse(" ").isLeft)
    assert(RelativeDirectory.parse("/tmp", _.startsWith("/")).isLeft)
    assert(PreviewUrl.parse("file:///tmp/site").isLeft)
    assert(PreviewUrl.parse("http://localhost:99999").isLeft)
    assert(PreviewUrl.parse("http://127.0.0.1:0").isLeft)
    assert(PreviewUrl.parse("http://bad host:4242").isLeft)
    assert(ProcessArgument.parse("bad\u0000argument", 0).isLeft)
    assert(StartupTimeout.parse(0).isLeft)
    assert(WorkspaceId.parse("").isLeft)
    assert(WorkspaceId.parse("workspace").isLeft)
    assert(WorkspaceId.parse("file:///work space").isLeft)
    assert(WorkspaceId.parse("file:///workspace\u0000").isLeft)
    assert(WorkspaceId.parse("file:///workspace\u0001").isLeft)
    assert(WorkspaceId.parse("file:///workspace\u0085").isLeft)

  test("parses a workspace identity once at the boundary"):
    assertEquals(
      WorkspaceId.parse("file:///workspace").map(_.value),
      Right("file:///workspace"),
    )
    assertEquals(
      WorkspaceId
        .parse("vscode-remote://ssh-remote+host/work")
        .map(_.value),
      Right("vscode-remote://ssh-remote+host/work"),
    )

  test("only creates a working directory contained by the workspace"):
    val contained =
      WorkingDirectory.parseResolved(
        "/workspace/docs",
        "docs",
        posixPaths,
      )
    val escaped =
      WorkingDirectory.parseResolved(
        "/outside",
        "../outside",
        posixPaths,
      )
    val unresolved =
      WorkingDirectory.parseResolved(
        "workspace/docs",
        "docs",
        posixPaths,
      )
    assertEquals(contained.map(_.value), Right("/workspace/docs"))
    assert(escaped.isLeft)
    assert(unresolved.isLeft)

  test("renders a synthesized process error before generic exit status"):
    val exit = ProcessExit.Failed(
      "workspace IPC connection ended",
      termination = None,
    )
    assertEquals(exit.render, "workspace IPC connection ended")

  test("translates every raw process observation into one explicit state"):
    assertEquals(
      ProcessExit.fromObservation(Some(0), None, None),
      ProcessExit.Terminated(ProcessTermination.Code(0)),
    )
    assertEquals(
      ProcessExit.fromObservation(None, Some("SIGTERM"), None),
      ProcessExit.Terminated(ProcessTermination.Signal("SIGTERM")),
    )
    assertEquals(
      ProcessExit.fromObservation(Some(1), None, Some("spawn failed")),
      ProcessExit.Failed(
        "spawn failed",
        Some(ProcessTermination.Code(1)),
      ),
    )
    assertEquals(
      ProcessExit.fromObservation(Some(1), Some("SIGTERM"), None),
      ProcessExit.Terminated(ProcessTermination.Ambiguous(1, "SIGTERM")),
    )
    assertEquals(
      ProcessExit.fromObservation(None, None, None),
      ProcessExit.Unknown,
    )
    assertEquals(
      ProcessExit
        .Terminated(ProcessTermination.Ambiguous(1, "SIGTERM"))
        .render,
      "exit code 1 and signal SIGTERM",
    )
    assertEquals(
      ProcessExit.Terminated(ProcessTermination.Code(0)).render,
      "exit code 0",
    )
    assertEquals(
      ProcessExit.Terminated(ProcessTermination.Signal("SIGTERM")).render,
      "signal SIGTERM",
    )
    assertEquals(
      ProcessExit
        .Failed("spawn failed", Some(ProcessTermination.Code(1)))
        .render,
      "spawn failed (exit code 1)",
    )
    assertEquals(
      ProcessExit.Unknown.render,
      "an unknown status",
    )

  test("parses only positive site revisions"):
    val accepted = for
      previous <- SiteRevision.parse(1L)
      current  <- SiteRevision.parse(2L)
    yield current.isAfter(previous)

    assertEquals(accepted, Right(true))
    assertEquals(SiteRevision.parse(1L).map(_.value), Right(1L))
    assertEquals(
      SiteRevision.parse(0L),
      Left(SiteRevisionProblem.NonPositive(0L)),
    )
    assertEquals(
      SiteRevision.parse(-1L),
      Left(SiteRevisionProblem.NonPositive(-1L)),
    )

  test("applies preview status transitions as pure domain rules"):
    val url           = right(PreviewUrl.parse("http://127.0.0.1:4242"))
    val otherUrl      = right(PreviewUrl.parse("http://127.0.0.1:4243"))
    val revision      = right(SiteRevision.parse(2L))
    val laterRevision = right(SiteRevision.parse(3L))
    val terminal      = PreviewTerminal.Exited(
      ProcessExit.Terminated(ProcessTermination.Code(0)),
    )
    val laterTerminal = PreviewTerminal.Failed("later failure")

    assertEquals(
      PreviewStatusTransition.ready(PreviewStatus.Starting, url),
      PreviewStatus.Ready(url, None) -> ReadyAcceptance.Accepted,
    )
    val ready = PreviewStatus.Ready(url, Some(revision))
    assertEquals(
      PreviewStatusTransition.ready(ready, otherUrl),
      ready -> ReadyAcceptance.Accepted,
    )
    List(PreviewStatus.Stopping, PreviewStatus.Terminal(terminal)).foreach:
      current =>
        assertEquals(
          PreviewStatusTransition.ready(current, otherUrl),
          current -> ReadyAcceptance.Rejected,
        )
    assertEquals(
      PreviewStatusTransition.refresh(
        PreviewStatus.Ready(url, None),
        revision,
      ),
      PreviewStatus.Ready(url, Some(revision)) ->
        Some(PreviewStatus.Ready(url, Some(revision))),
    )
    val laterReady = PreviewStatus.Ready(url, Some(laterRevision))
    assertEquals(
      PreviewStatusTransition.refresh(ready, laterRevision),
      laterReady -> Some(laterReady),
    )
    List(
      PreviewStatus.Starting,
      PreviewStatus.Stopping,
      PreviewStatus.Terminal(terminal),
    ).foreach: current =>
      assertEquals(
        PreviewStatusTransition.refresh(current, revision),
        current -> None,
      )
    assertEquals(
      PreviewStatusTransition.terminal(PreviewStatus.Stopping, terminal),
      PreviewStatus.Stopping -> TerminalAcceptance.Rejected,
    )
    assertEquals(
      PreviewStatusTransition.terminal(PreviewStatus.Starting, terminal),
      PreviewStatus.Terminal(terminal) -> TerminalAcceptance.Accepted,
    )
    List(ready, PreviewStatus.Terminal(terminal)).foreach: current =>
      assertEquals(
        PreviewStatusTransition.terminal(current, terminal),
        PreviewStatus.Terminal(terminal) -> TerminalAcceptance.Accepted,
      )
    assertEquals(
      PreviewStatusTransition.terminal(
        PreviewStatus.Terminal(terminal),
        laterTerminal,
      ),
      PreviewStatus.Terminal(laterTerminal) -> TerminalAcceptance.Accepted,
    )
    assertEquals(
      PreviewStatusTransition.stopping(PreviewStatus.Starting),
      PreviewStatus.Stopping,
    )
    List(
      PreviewStatus.Stopping,
      PreviewStatus.Ready(url, Some(revision)),
      PreviewStatus.Terminal(terminal),
    ).foreach(status =>
      assertEquals(
        PreviewStatusTransition.stopping(status),
        PreviewStatus.Stopping,
      ),
    )
