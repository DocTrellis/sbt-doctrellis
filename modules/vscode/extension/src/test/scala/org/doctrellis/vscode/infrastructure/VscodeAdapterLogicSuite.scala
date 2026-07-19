package org.doctrellis.vscode.infrastructure

import munit.FunSuite
import org.doctrellis.vscode.domain.Workspace

class VscodeAdapterLogicSuite extends FunSuite:
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

  test("trust validation gates only commands that require trust"):
    val required = WorkspaceTrust.validate(required = true, trusted = false)
    val cleanup  = WorkspaceTrust.validate(required = false, trusted = false)
    val trusted  = WorkspaceTrust.validate(required = true, trusted = true)

    assert(required.isLeft)
    assertEquals(cleanup, Right(()))
    assertEquals(trusted, Right(()))

  private def workspace(root: String, value: String): Workspace =
    Workspace(value, "docs", root, value)
