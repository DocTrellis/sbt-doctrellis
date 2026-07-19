package org.doctrellis.sbtplugin

import java.nio.file.{Files, LinkOption}

class MdocProcessorSuite extends munit.FunSuite:
  test("skips directory symlinks while preserving file symlink inputs"):
    if !isWindows then
      val root     = Files.createTempDirectory("doctrellis-mdoc-links-")
      val source   = Files.createDirectory(root.resolve("docs"))
      val output   = Files.createDirectory(root.resolve("output"))
      val external = Files.createDirectory(root.resolve("external"))
      try
        Files.writeString(external.resolve("outside.txt"), "outside")
        Files.createSymbolicLink(source.resolve("linked-directory"), external)
        Files.createSymbolicLink(
          source.resolve("linked-file.txt"),
          external.resolve("outside.txt"),
        )

        val markdownCount = MdocProcessor.processSourceTree(source, output):
          (_, _, _) => fail("unexpected Markdown input")

        assertEquals(markdownCount, 0)
        assert(
          !Files.exists(
            output.resolve("linked-directory"),
            LinkOption.NOFOLLOW_LINKS,
          ),
        )
        assertEquals(
          Files.readString(output.resolve("linked-file.txt")),
          "outside",
        )
        assert(!Files.isSymbolicLink(output.resolve("linked-file.txt")))
        assertEquals(
          Files.readString(external.resolve("outside.txt")),
          "outside",
        )
      finally GeneratedDirectory.delete(root)

  private def isWindows: Boolean =
    System
      .getProperty("os.name", "")
      .toLowerCase(java.util.Locale.ROOT)
      .contains("win")
