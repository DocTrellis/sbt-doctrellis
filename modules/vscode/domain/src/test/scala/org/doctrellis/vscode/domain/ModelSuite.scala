package org.doctrellis.vscode.domain

class ModelSuite extends munit.FunSuite:
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
    assert(PreviewUrl.parse("http://bad host:4242").isLeft)
    assert(ProcessArgument.parse("bad\u0000argument", 0).isLeft)
    assert(StartupTimeout.parse(0).isLeft)

  test("only creates a working directory contained by the workspace"):
    val contained =
      WorkingDirectory.parseResolved("/workspace/docs", "docs", "/", false)
    val escaped =
      WorkingDirectory.parseResolved("/outside", "../outside", "/", false)
    assertEquals(contained.map(_.value), Right("/workspace/docs"))
    assert(escaped.isLeft)
