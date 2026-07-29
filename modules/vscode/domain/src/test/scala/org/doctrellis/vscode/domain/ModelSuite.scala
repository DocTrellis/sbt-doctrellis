package org.doctrellis.vscode.domain

class ModelSuite extends munit.FunSuite:
  private val posixPaths = new PathSemantics("/", _.startsWith("/"))

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
    val exit = ProcessExit(
      code = None,
      signal = None,
      error = Some("workspace IPC connection ended"),
    )
    assertEquals(exit.render, "workspace IPC connection ended")

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
