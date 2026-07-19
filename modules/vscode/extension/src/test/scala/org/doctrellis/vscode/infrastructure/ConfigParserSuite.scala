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

  private val dynamic =
    """{
      |  "version": 2,
      |  "preview": {
      |    "command": "sbt",
      |    "args": ["--server", "--batch", "set doctrellisPort := 0", "doctrellisPreview"],
      |    "workingDirectory": ".",
      |    "endpoint": {
      |      "source": "ipc",
      |      "protocol": "doctrellis-preview-v1"
      |    },
      |    "startupTimeoutMs": 90000
      |  }
      |}""".stripMargin

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

  test("consumes schema version 2 as an announced IPC endpoint"):
    val parsed = ConfigParser.parse(dynamic, isAbsolute)
    assertEquals(parsed.map(_.endpoint), Right(PreviewEndpoint.Announced))
    assertEquals(
      parsed.map(_.arguments.map(_.value)),
      Right(
        Vector(
          "--server",
          "--batch",
          "set doctrellisPort := 0",
          "doctrellisPreview",
        ),
      ),
    )

  test("rejects unsupported versions and unexpected fields"):
    val version = ConfigParser.parse(
      valid.replace("\"version\": 1", "\"version\": 3"),
      isAbsolute,
    )
    val extra = ConfigParser.parse(
      valid.replace("\"version\": 1", "\"version\": 1, \"extra\": true"),
      isAbsolute,
    )
    assertEquals(
      version.left.map(_.message),
      Left("version must be 1 or 2, but was 3"),
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

  test("keeps version-specific endpoint fields exclusive"):
    val fixedInVersionTwo = ConfigParser.parse(
      dynamic.replace(
        "\"endpoint\": {\n      \"source\": \"ipc\",\n      \"protocol\": \"doctrellis-preview-v1\"\n    }",
        "\"url\": \"http://127.0.0.1:4242\"",
      ),
      isAbsolute,
    )
    val announcedInVersionOne = ConfigParser.parse(
      valid.replace(
        "\"url\": \"http://127.0.0.1:4242\"",
        "\"endpoint\": {\"source\": \"ipc\", \"protocol\": \"doctrellis-preview-v1\"}",
      ),
      isAbsolute,
    )
    assert(fixedInVersionTwo.isLeft)
    assert(announcedInVersionOne.isLeft)

  test("rejects unknown version 2 endpoint source and protocol"):
    val source = ConfigParser.parse(
      dynamic.replace("\"source\": \"ipc\"", "\"source\": \"url\""),
      isAbsolute,
    )
    val protocol = ConfigParser.parse(
      dynamic.replace("doctrellis-preview-v1", "doctrellis-preview-v2"),
      isAbsolute,
    )
    assert(source.isLeft)
    assert(protocol.isLeft)
