package org.doctrellis.vscode.infrastructure

import org.doctrellis.vscode.domain.*

class ConfigParserSuite extends munit.FunSuite:
  private val valid =
    """{
      |  "version": 1,
      |  "preview": {
      |    "command": "sbt",
      |    "args": ["doctrellisPreview"],
      |    "workingDirectory": ".",
      |    "url": "http://127.0.0.1:4242",
      |    "startupTimeoutMs": 90000
      |  }
      |}""".stripMargin

  private val isAbsolute: String => Boolean = _.startsWith("/")

  test("consumes schema version 1 into a parsed preview template"):
    val parsed = ConfigParser.parse(valid, isAbsolute)
    assertEquals(parsed.map(_.executable.value), Right("sbt"))
    assertEquals(
      parsed.map(_.arguments.map(_.value)),
      Right(Vector("doctrellisPreview")),
    )

  test("rejects args represented as a shell string"):
    val parsed = ConfigParser.parse(
      valid.replace("[\"doctrellisPreview\"]", "\"doctrellisPreview\""),
      isAbsolute,
    )
    assertEquals(
      parsed.left.map(_.message),
      Left("preview.args must be an array of strings"),
    )

  test("rejects unsupported versions and unexpected fields"):
    val version = ConfigParser.parse(
      valid.replace("\"version\": 1", "\"version\": 2"),
      isAbsolute,
    )
    val extra = ConfigParser.parse(
      valid.replace("\"version\": 1", "\"version\": 1, \"extra\": true"),
      isAbsolute,
    )
    assertEquals(
      version.left.map(_.message),
      Left("version must be 1, but was 2"),
    )
    assertEquals(extra.left.map(_.message), Left("unexpected field: extra"))

  test("rejects absolute working directories and non-HTTP URLs"):
    val absolute = ConfigParser.parse(
      valid.replace(
        "\"workingDirectory\": \".\"",
        "\"workingDirectory\": \"/tmp\"",
      ),
      isAbsolute,
    )
    val fileUrl = ConfigParser.parse(
      valid.replace("http://127.0.0.1:4242", "file:///tmp/site"),
      isAbsolute,
    )
    assert(absolute.isLeft)
    assert(fileUrl.isLeft)

  test("rejects arguments that cannot be passed to spawn"):
    val parsed = ConfigParser.parse(
      valid.replace("doctrellisPreview", "bad\\u0000argument"),
      isAbsolute,
    )
    assertEquals(
      parsed.left.map(_.message),
      Left("preview.args[0] must not contain a NUL character"),
    )
