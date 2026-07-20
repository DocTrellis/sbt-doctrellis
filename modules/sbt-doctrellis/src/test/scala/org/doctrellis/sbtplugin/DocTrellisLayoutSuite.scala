package org.doctrellis.sbtplugin

import java.nio.file.{Files, Path}

import scala.jdk.CollectionConverters.*
import scala.util.Using

class DocTrellisLayoutSuite extends munit.FunSuite:
  test(
    "a canonical layout accepts generated directories outside the workspace",
  ):
    withFixture: fixture =>
      Files.createDirectories(fixture.mdoc)
      Files.createDirectories(fixture.site)

      val layout = right(fixture.parse())

      assertEquals(layout.workspaceDirectory, fixture.workspace.toRealPath())
      assertEquals(layout.sourceDirectory, fixture.source.toRealPath())
      assertEquals(layout.mdocDirectory, fixture.mdoc.toRealPath())
      assertEquals(layout.siteDirectory, fixture.site.toRealPath())

  test("an mdoc directory equal to the source is rejected without mutation"):
    withFixture: fixture =>
      val sentinel = fixture.source.resolve("index.md")
      Files.writeString(sentinel, "source sentinel")

      val problem = left(fixture.parse(mdoc = fixture.source))

      problem match
        case DocTrellisLayoutProblem.OverlappingDirectories(
              DocTrellisDirectoryRole.Mdoc,
              generated,
              DocTrellisDirectoryRole.Source,
              source,
            ) =>
          assertEquals(generated, fixture.source.toRealPath())
          assertEquals(source, fixture.source.toRealPath())
        case other => fail(s"unexpected layout problem: $other")
      assertEquals(Files.readString(sentinel), "source sentinel")

  test("a generated directory must not contain the workspace"):
    withFixture: fixture =>
      val problem = left(fixture.parse(mdoc = fixture.root))

      problem match
        case DocTrellisLayoutProblem.GeneratedDirectoryContainsWorkspace(
              DocTrellisDirectoryRole.Mdoc,
              generated,
              workspace,
            ) =>
          assertEquals(generated, fixture.root.toRealPath())
          assertEquals(workspace, fixture.workspace.toRealPath())
        case other => fail(s"unexpected layout problem: $other")

  test("a generated directory must not contain an external source"):
    withFixture: fixture =>
      val content = Files.createDirectories(fixture.root.resolve("content"))
      val source  = Files.createDirectories(content.resolve("docs"))
      val problem = left(fixture.parse(source = source, mdoc = content))

      problem match
        case DocTrellisLayoutProblem.OverlappingDirectories(
              DocTrellisDirectoryRole.Mdoc,
              _,
              DocTrellisDirectoryRole.Source,
              _,
            ) =>
          ()
        case other => fail(s"unexpected layout problem: $other")

  test("the site directory must not equal or nest within the mdoc directory"):
    withFixture: fixture =>
      Files.createDirectories(fixture.mdoc)
      Vector(fixture.mdoc, fixture.mdoc.resolve("site")).foreach: site =>
        val problem = left(fixture.parse(site = site))

        problem match
          case DocTrellisLayoutProblem.OverlappingDirectories(
                DocTrellisDirectoryRole.Mdoc,
                _,
                DocTrellisDirectoryRole.Site,
                _,
              ) =>
            ()
          case other => fail(s"unexpected layout problem for $site: $other")

  test(
    "a symlink alias of the source is rejected where symlinks are supported",
  ):
    assume(!isWindows, "requires Unix symbolic-link semantics")
    withFixture: fixture =>
      val alias = fixture.root.resolve("source-alias")
      Files.createSymbolicLink(alias, fixture.source)

      val problem = left(fixture.parse(mdoc = alias))

      problem match
        case DocTrellisLayoutProblem.OverlappingDirectories(
              DocTrellisDirectoryRole.Mdoc,
              generated,
              DocTrellisDirectoryRole.Source,
              source,
            ) =>
          assertEquals(generated, fixture.source.toRealPath())
          assertEquals(source, fixture.source.toRealPath())
        case other => fail(s"unexpected layout problem: $other")

  test(
    "a lexical source symlink inside the canonical mdoc target is rejected",
  ):
    assume(!isWindows, "requires Unix symbolic-link semantics")
    withFixture: fixture =>
      val generatedTarget = Files.createDirectories(
        fixture.root.resolve("generated-target"),
      )
      val generatedSentinel = generatedTarget.resolve("generated-sentinel")
      Files.writeString(generatedSentinel, "generated sentinel")
      val mdocAlias = fixture.root.resolve("mdoc-alias")
      Files.createSymbolicLink(mdocAlias, generatedTarget)

      val externalSource = Files.createDirectories(
        fixture.root.resolve("external-source"),
      )
      val sourceSentinel = externalSource.resolve("source-sentinel")
      Files.writeString(sourceSentinel, "source sentinel")
      val sourceLink = generatedTarget.resolve("source-link")
      Files.createSymbolicLink(sourceLink, externalSource)

      val problem = left(
        fixture.parse(source = sourceLink, mdoc = mdocAlias),
      )

      problem match
        case DocTrellisLayoutProblem.OverlappingDirectories(
              DocTrellisDirectoryRole.Mdoc,
              generated,
              DocTrellisDirectoryRole.Source,
              source,
            ) =>
          assertEquals(generated, generatedTarget.toRealPath())
          assertEquals(source, externalSource.toRealPath())
        case other => fail(s"unexpected layout problem: $other")
      assert(Files.isSymbolicLink(mdocAlias))
      assert(Files.isSymbolicLink(sourceLink))
      assertEquals(Files.readString(generatedSentinel), "generated sentinel")
      assertEquals(Files.readString(sourceSentinel), "source sentinel")
      assertEquals(
        Files.readString(sourceLink.resolve("source-sentinel")),
        "source sentinel",
      )

  test("a missing source is rejected before a generated directory is changed"):
    withFixture: fixture =>
      val missing  = fixture.workspace.resolve("missing-docs")
      val sentinel = Files.createDirectories(fixture.mdoc).resolve("sentinel")
      Files.writeString(sentinel, "generated sentinel")

      val problem = left(fixture.parse(source = missing))

      assertEquals(
        problem,
        DocTrellisLayoutProblem.MissingDirectory(
          DocTrellisDirectoryRole.Source,
          missing.toFile.getCanonicalFile.toPath,
        ),
      )
      assertEquals(Files.readString(sentinel), "generated sentinel")

  private final case class Fixture(
      root: Path,
      workspace: Path,
      source: Path,
      mdoc: Path,
      site: Path,
  ):
    def parse(
        source: Path = source,
        mdoc: Path = mdoc,
        site: Path = site,
    ): Either[DocTrellisLayoutProblem, DocTrellisLayout] =
      DocTrellisLayout.parse(
        workspaceDirectory = workspace,
        sourceDirectory = source,
        mdocDirectory = mdoc,
        siteDirectory = site,
      )

  private def withFixture(action: Fixture => Unit): Unit =
    val root      = Files.createTempDirectory("doctrellis-layout-")
    val workspace = Files.createDirectories(root.resolve("workspace"))
    val source    = Files.createDirectories(workspace.resolve("docs"))
    val generated = Files.createDirectories(root.resolve("generated"))
    val fixture   = Fixture(
      root = root,
      workspace = workspace,
      source = source,
      mdoc = generated.resolve("mdoc"),
      site = generated.resolve("site"),
    )
    try action(fixture)
    finally deleteTree(root)

  private def deleteTree(root: Path): Unit =
    if Files.exists(root) then
      Using.resource(Files.walk(root)): paths =>
        paths.iterator().asScala.toVector.reverse.foreach(Files.deleteIfExists)

  private def left(
      result: Either[DocTrellisLayoutProblem, DocTrellisLayout],
  ): DocTrellisLayoutProblem = result match
    case Left(problem) => problem
    case Right(layout) =>
      fail(s"expected an invalid layout, but parsed $layout")

  private def right(
      result: Either[DocTrellisLayoutProblem, DocTrellisLayout],
  ): DocTrellisLayout = result match
    case Left(problem) => fail(problem.message)
    case Right(layout) => layout

  private def isWindows: Boolean =
    System
      .getProperty("os.name", "")
      .toLowerCase(java.util.Locale.ROOT)
      .contains("win")
