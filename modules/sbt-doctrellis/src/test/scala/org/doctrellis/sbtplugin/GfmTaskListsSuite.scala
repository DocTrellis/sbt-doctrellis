package org.doctrellis.sbtplugin

import laika.api.Transformer
import laika.format.{HTML, Markdown}

class GfmTaskListsSuite extends munit.FunSuite:

  private val transformer = Transformer
    .from(Markdown)
    .to(HTML)
    .using(Markdown.GitHubFlavor, GfmTaskLists)
    .build

  test("renders unchecked and case-insensitive checked task markers"):
    val html = render(
      """- [ ] todo
        |- [x] done
        |- [X] also done
        |""".stripMargin,
    )

    assertEquals(occurrences(html, "type=\"checkbox\""), 3)
    assertEquals(occurrences(html, "disabled=\"disabled\""), 3)
    assertEquals(occurrences(html, "checked=\"checked\""), 2)
    assertEquals(occurrences(html, "aria-label=\"Incomplete task\""), 1)
    assertEquals(occurrences(html, "aria-label=\"Completed task\""), 2)
    assert(html.contains("todo"))
    assert(html.contains("done"))
    assert(html.contains("also done"))

  test("represents task markers as dedicated AST elements"):
    val document = transformer.parser.parse("- [ ] todo\n- [x] done") match
      case Right(value) => value
      case Left(error)  => fail(error.toString)

    val states = document.content.collect:
      case checkbox: TaskListCheckbox => checkbox.checked
    val candidates = document.content.collect:
      case candidate: TaskListCandidate => candidate

    assertEquals(states, List(false, true))
    assertEquals(candidates, Nil)

  test("supports bullet, ordered, nested, and mixed lists"):
    val html = render(
      """- [ ] parent
        |  - [x] nested child
        |- ordinary item
        |
        |1. [ ] ordered unchecked
        |2. [x] ordered checked
        |3. ordinary ordered item
        |""".stripMargin,
    )

    assertEquals(occurrences(html, "type=\"checkbox\""), 4)
    assertEquals(occurrences(html, "class=\"task-list-item\""), 4)
    assert(html.contains("contains-task-list"))
    assert(html.contains("arabic"))
    assert(html.contains("ordinary item"))
    assert(html.contains("ordinary ordered item"))
    assert(html.indexOf("nested child") < html.indexOf("</ul>"))

  test("keeps GFM-width indentation nested in ordered task lists"):
    val html = render(
      """1. [ ] ordered parent
        |   1. [x] ordered child
        |""".stripMargin,
    )

    assertEquals(occurrences(html, "type=\"checkbox\""), 2)
    assert(html.indexOf("ordered child") < html.indexOf("</ol>"))

  test("keeps ordinary and task items nested under either kind of parent"):
    val taskParent = render(
      """- [ ] task parent
        |  - ordinary child
        |  - [x] task child
        |""".stripMargin,
    )
    val ordinaryParent = render(
      """- ordinary parent
        |  - [x] nested task
        |""".stripMargin,
    )

    assertEquals(occurrences(taskParent, "<ul"), 2)
    assertEquals(occurrences(taskParent, "type=\"checkbox\""), 2)
    assertEquals(occurrences(ordinaryParent, "<ul"), 2)
    assertEquals(occurrences(ordinaryParent, "type=\"checkbox\""), 1)

  test("does not reparent a separately indented root task item"):
    val markdown = "- [ ] first\n\n - [x] second\n"
    val html     = render(markdown)

    assertEquals(GfmListIndentation.normalize(markdown), markdown)
    assert(html.indexOf("</li>") < html.indexOf("second"))

  test("normalizes CRLF nesting and preserves fenced task-like code"):
    val nested = render("- [ ] parent\r\n  - [x] child\r\n")
    val fenced = render(
      """- [ ] parent
        |  ```text
        |  - [x] fenced literal
        |  ```
        |""".stripMargin,
    )

    assertEquals(occurrences(nested, "<ul"), 2)
    assertEquals(occurrences(nested, "type=\"checkbox\""), 2)
    assertEquals(occurrences(fenced, "type=\"checkbox\""), 1)
    assert(fenced.contains("- [x] fenced literal"))

  test("preserves GFM-width continuation text in a task item"):
    val html = render(
      """- [ ] first line
        |  - ordinary child
        |  continued parent line
        |  - [x] second child
        |""".stripMargin,
    )

    assertEquals(occurrences(html, "<ul"), 2)
    assertEquals(occurrences(html, "type=\"checkbox\""), 2)
    assert(html.contains("continued parent line"))
    assert(!html.contains("<pre"))

  test("keeps parent-level fenced blocks and following children nested"):
    val html = render(
      """- [ ] parent
        |  - ordinary child
        |  ```text
        |  fenced content
        |  ```
        |  - [x] second child
        |""".stripMargin,
    )

    assertEquals(occurrences(html, "<ul"), 2)
    assertEquals(occurrences(html, "type=\"checkbox\""), 2)
    assert(html.contains("fenced content"))

  test("does not open a fence indented four spaces past list content"):
    val html = render(
      """- ordinary parent
        |
        |      ```text
        |  - [x] nested child
        |""".stripMargin,
    )

    assertEquals(occurrences(html, "<ul"), 2)
    assertEquals(occurrences(html, "type=\"checkbox\""), 1)
    assert(html.contains("nested child"))

  test("does not close a fence indented four spaces past list content"):
    val html = render(
      """- ordinary parent
        |  ```text
        |      ```
        |  - [x] fenced literal
        |  ```
        |  - [x] nested child
        |""".stripMargin,
    )

    assertEquals(occurrences(html, "type=\"checkbox\""), 1)
    assert(html.contains("- [x] fenced literal"))
    assert(html.contains("nested child"))

  test("does not reinterpret indented code as nested task lists"):
    val bullet = render(
      """- [ ] task
        |
        |      - [x] bullet code
        |""".stripMargin,
    )
    val ordered = render(
      """1. [ ] task
        |
        |       1. [x] ordered code
        |""".stripMargin,
    )
    val topLevelCode = "    - [x] code\n      - ordinary code\n"

    assertEquals(occurrences(bullet, "type=\"checkbox\""), 1)
    assertEquals(occurrences(ordered, "type=\"checkbox\""), 1)
    assert(bullet.contains("<pre"))
    assert(ordered.contains("<pre"))
    assertEquals(GfmListIndentation.normalize(topLevelCode), topLevelCode)
    assertEquals(occurrences(render(topLevelCode), "type=\"checkbox\""), 0)

  test("leaves documents without task list items byte-for-byte unchanged"):
    val markdown =
      "ordinary\r\n\r\n- parent\r\n  - child\r\n\r\n```text\r\n- [x] code\r\n```\r\n"

    assertEquals(GfmListIndentation.normalize(markdown), markdown)

  test("preserves inline Markdown after a task marker"):
    val html = render(
      """- [ ] *emphasis* and **strong**
        |- [x] [`doctrellisBuild`](https://doctrellis.org) and `inline code`
        |""".stripMargin,
    )

    assert(html.contains("<em>emphasis</em>"))
    assert(html.contains("<strong>strong</strong>"))
    assert(html.contains("<code>doctrellisBuild</code>"))
    assert(html.contains("<code>inline code</code>"))
    assert(html.contains("href=\"https://doctrellis.org\""))

  test("handles LF and CRLF input"):
    val inputs = Seq(
      "- [ ] todo\n- [x] done\n",
      "- [ ] todo\r\n- [x] done\r\n",
    )

    inputs.foreach: input =>
      val html = render(input)
      assertEquals(occurrences(html, "type=\"checkbox\""), 2)
      assertEquals(occurrences(html, "checked=\"checked\""), 1)

  test("does not recognize marker-like text outside the first list content"):
    val html = render(
      """Body text with [ ] in the middle.
        |
        |`- [ ] inline code`
        |
        |```text
        |- [x] fenced code
        |```
        |
        |- \[ ] escaped marker
        |- prefix [ ] is not a task marker
        |
        |[x](https://example.com) is a normal link.
        |
        |[x] is a shortcut reference link.
        |
        |![logo](https://example.com/logo.png)
        |
        |[x]: https://example.com/reference
        |""".stripMargin,
    )

    assertEquals(occurrences(html, "type=\"checkbox\""), 0)
    assert(html.contains("Body text with [ ] in the middle."))
    assert(html.contains("- [ ] inline code"))
    assert(html.contains("- [x] fenced code"))
    assert(html.contains("[ ] escaped marker"))
    assert(html.contains("prefix [ ] is not a task marker"))
    assert(html.contains("href=\"https://example.com\""))
    assert(html.contains("href=\"https://example.com/reference\""))
    assert(html.contains("src=\"https://example.com/logo.png\""))

  test("requires the marker to be followed by a GFM boundary"):
    val ordinaryLink = render(
      "- [x]not a task\n\n[x]: https://example.com/reference\n",
    )

    assertEquals(occurrences(ordinaryLink, "type=\"checkbox\""), 0)
    assert(ordinaryLink.contains("href=\"https://example.com/reference\""))
    assertEquals(occurrences(render("- [ ]"), "type=\"checkbox\""), 1)

  test("keeps unrelated broken link validation enabled"):
    transformer.transform("[broken][missing]") match
      case Left(error) =>
        assert(
          error.toString.toLowerCase.contains("unresolved link id reference"),
        )
      case Right(html) => fail(s"expected link validation to fail, got: $html")

  private def render(markdown: String): String =
    transformer.transform(markdown) match
      case Right(value) => value
      case Left(error)  => fail(error.toString)

  private def occurrences(input: String, value: String): Int =
    input.sliding(value.length).count(_ == value)
