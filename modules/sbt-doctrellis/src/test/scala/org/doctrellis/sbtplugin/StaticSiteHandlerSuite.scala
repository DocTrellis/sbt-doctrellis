package org.doctrellis.sbtplugin

import java.nio.file.Files

class StaticSiteHandlerSuite extends munit.FunSuite:
  test("resolves the root and nested directories to index.html"):
    val root   = Files.createTempDirectory("doctrellis-handler")
    val nested = Files.createDirectories(root.resolve("guide"))
    Files.writeString(root.resolve("index.html"), "root")
    Files.writeString(nested.resolve("index.html"), "nested")

    assertEquals(
      StaticSiteHandler.resolve(root, "/"),
      Some(root.resolve("index.html").toRealPath()),
    )
    assertEquals(
      StaticSiteHandler.resolve(root, "/guide/"),
      Some(nested.resolve("index.html").toRealPath()),
    )

  test("does not resolve traversal outside the site root"):
    val root = Files.createTempDirectory("doctrellis-handler")
    assertEquals(StaticSiteHandler.resolve(root, "/../secret.txt"), None)
    assertEquals(StaticSiteHandler.resolve(root, "/%2e%2e/secret.txt"), None)
    assertEquals(StaticSiteHandler.resolve(root, "/%zz"), None)

  test("does not follow site symlinks outside the root"):
    val root    = Files.createTempDirectory("doctrellis-handler")
    val outside = Files.createTempFile("doctrellis-secret", ".txt")
    Files.createSymbolicLink(root.resolve("secret.txt"), outside)
    assertEquals(StaticSiteHandler.resolve(root, "/secret.txt"), None)

  test("uses explicit UTF-8 content types"):
    assertEquals(
      StaticSiteHandler.contentType(java.nio.file.Path.of("index.html")),
      "text/html; charset=utf-8",
    )
    assertEquals(
      StaticSiteHandler.contentType(java.nio.file.Path.of("site.css")),
      "text/css; charset=utf-8",
    )
