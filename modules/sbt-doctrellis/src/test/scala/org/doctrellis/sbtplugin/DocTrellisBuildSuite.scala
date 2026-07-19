package org.doctrellis.sbtplugin

import java.nio.file.Files
import java.util.UUID

import sbt.util.LogExchange

class DocTrellisBuildSuite extends munit.FunSuite:
  test(
    "a live preview blocks a public build before it resets either output",
  ):
    withLayout: layout =>
      Files.createDirectories(layout.mdocDirectory)
      Files.createDirectories(layout.siteDirectory)
      Files.writeString(layout.mdocDirectory.resolve("index.md"), "old mdoc")
      Files.writeString(layout.siteDirectory.resolve("index.html"), "old site")
      val preview = PreviewPublication.open(layout, logger)
      try
        val failure = intercept[IllegalStateException](
          program(layout).buildPublic(),
        )
        assert(failure.getMessage.contains("build or preview"))
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "old mdoc",
        )
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "old site",
        )
      finally preview.close()

  test("a public build lease excludes previews and is released on failure"):
    withLayout: layout =>
      val buildProgram = program(layout)
      val expected     = IllegalStateException("injected build failure")

      val actual = intercept[IllegalStateException]:
        buildProgram.withLockedPublicTarget: target =>
          assertEquals(target, buildProgram.publicTarget)
          val blocked = intercept[IllegalStateException](
            PreviewPublication.open(layout, logger),
          )
          assert(blocked.getMessage.contains("build or preview"))
          throw expected

      assertEquals(actual, expected)
      val afterRelease = PreviewPublication.open(layout, logger)
      afterRelease.close()

  test("a public build reconciles stale preview backups before replacing them"):
    withLayout: layout =>
      val destination = layout.mdocDirectory.toAbsolutePath.normalize()
      val sessionId   = UUID.randomUUID().toString
      val prefix      = s".${destination.getFileName}.doctrellis-$sessionId"
      val backup      = destination.getParent.resolve(s"$prefix.backup")
      val marker      = destination.getParent.resolve(s"$prefix.owner")
      Files.createDirectories(backup)
      Files.writeString(backup.resolve("index.md"), "last good mdoc")
      Files.writeString(
        marker,
        s"doctrellis-preview-publication-v1\nsession=$sessionId\ntarget=$destination\n",
      )

      var recoveredBeforeBuild = Option.empty[String]
      program(layout).withLockedPublicTarget: target =>
        recoveredBeforeBuild = Some(
          Files.readString(target.mdocDirectory.resolve("index.md")),
        )
        GeneratedDirectory.reset(target.mdocDirectory)
        GeneratedDirectory.reset(target.siteDirectory)
        Files.writeString(target.mdocDirectory.resolve("index.md"), "new mdoc")
        Files.writeString(
          target.siteDirectory.resolve("index.html"),
          "new site",
        ): Unit

      assertEquals(recoveredBeforeBuild, Some("last good mdoc"))
      val nextPreview = PreviewPublication.open(layout, logger)
      try
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "new mdoc",
        )
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "new site",
        )
      finally nextPreview.close()

  private def program(layout: DocTrellisLayout): DocTrellisBuildProgram =
    DocTrellisBuildProgram(
      DocTrellisBuildInputs(
        layout = layout,
        classpath = Vector.empty,
        scalacOptions = Vector.empty,
        mdocArguments = Vector.empty,
        mdocVariables = Map.empty,
        log = logger,
      ),
    )

  private def withLayout(action: DocTrellisLayout => Unit): Unit =
    val root      = Files.createTempDirectory("doctrellis-public-build-")
    val workspace = Files.createDirectories(root.resolve("workspace"))
    val source    = Files.createDirectories(workspace.resolve("docs"))
    val output    = Files.createDirectories(root.resolve("output"))
    val layout    = DocTrellisLayout
      .parse(
        workspaceDirectory = workspace,
        sourceDirectory = source,
        mdocDirectory = output.resolve("mdoc"),
        siteDirectory = output.resolve("site"),
      )
      .fold(problem => fail(problem.message), identity)
    try action(layout)
    finally GeneratedDirectory.delete(root)

  private def logger = LogExchange.logger("doctrellis-build-test")
