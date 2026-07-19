package org.doctrellis.sbtplugin

import laika.api.bundle.{
  ExtensionBundle,
  ParserBundle,
  ParserHooks,
  SpanParserBuilder,
}
import laika.api.format.TagFormatter
import laika.ast.*
import laika.format.HTML
import laika.parse.SourceFragment
import laika.parse.builders.*
import laika.parse.syntax.*
import laika.parse.text.PrefixedParser

private[sbtplugin] object GfmTaskLists extends ExtensionBundle:

  private val TaskItemStyle    = "task-list-item"
  private val ContainsTaskList = "contains-task-list"

  val description: String = "GitHub-Flavored Markdown task lists"

  private val markerParser = SpanParserBuilder.standalone:
    PrefixedParser('['):
      val boundary = lookAhead(oneOf(' ', '\t', '\n', '\r').void | eof)
      ((("[" ~> oneOf(' ', 'x', 'X') <~ "]") <~ boundary).withCursor)
        .map: (state, source) =>
          TaskListCandidate(state != " ", s"[$state]", source)

  override val parsers: ParserBundle = new ParserBundle(
    spanParsers = Seq(markerParser),
    markupParserHooks = Some(
      new ParserHooks(
        postProcessBlocks = blocks => blocks.map(confirmTaskMarkers),
        preProcessInput = GfmListIndentation.normalize,
      ),
    ),
  )

  val themeCSS: String =
    """li.task-list-item {
      |  list-style-type: none;
      |}
      |
      |li.task-list-item > input[type="checkbox"],
      |li.task-list-item > p > input[type="checkbox"] {
      |  margin: 0 0.4em 0 -1.4em;
      |  vertical-align: middle;
      |}
      |""".stripMargin

  private val taskListRules = RewriteRules.forBlocks:
    case list: BulletList => RewriteAction.Replace(rewrite(list))
    case list: EnumList   => RewriteAction.Replace(rewrite(list))

  private val remainingCandidateRules = RewriteRules.forSpans:
    case candidate: TaskListCandidate if candidate.checked =>
      val text = candidate.marker.slice(1, 2)
      RewriteAction.Replace(
        LinkIdReference(
          Seq(Text(text)),
          text.toLowerCase(java.util.Locale.ROOT),
          candidate.source,
          candidate.options,
        ),
      )
    case candidate: TaskListCandidate =>
      RewriteAction.Replace(Text(candidate.marker, candidate.options))

  override val rewriteRules: RewriteRules.RewritePhaseBuilder =
    (taskListRules ++ remainingCandidateRules).asDefaultPhaseBuilder

  override val renderOverrides: Seq[HTML.Overrides] = Seq(
    HTML.Overrides:
      case (formatter, checkbox: TaskListCheckbox) =>
        val checkedAttribute =
          if checkbox.checked then Seq("checked" -> "checked") else Nil
        val attributes = Seq(
          "type"     -> "checkbox",
          "disabled" -> "disabled",
        ) ++ checkedAttribute ++ Seq(
          "aria-label" -> (if checkbox.checked then "Completed task"
                           else "Incomplete task"),
        )
        formatter.emptyElement("input", checkbox, attributes*)
      case (formatter, item: BulletListItem) if item.hasStyle(TaskItemStyle) =>
        renderTaskItem(formatter, item)
      case (formatter, item: EnumListItem) if item.hasStyle(TaskItemStyle) =>
        renderTaskItem(formatter, item)
      case (formatter, list: EnumList) if list.hasStyle(ContainsTaskList) =>
        val enumStyle = list.format.enumType.toString.toLowerCase(
          java.util.Locale.ROOT,
        )
        val attributes = formatter.optAttributes(
          "start" -> Option.when(list.start != 1)(list.start.toString),
        )
        formatter.indentedElement(
          "ol",
          list.withStyle(enumStyle),
          attributes*,
        ),
  )

  private def renderTaskItem(
      formatter: TagFormatter,
      item: BlockContainer,
  ): String = item.content match
    case Seq(spans: SpanSequence) =>
      formatter.element("li", spans.withOptions(item.options))
    case Seq(Paragraph(spans, options)) =>
      formatter.element(
        "li",
        SpanSequence(spans, options + item.options),
      )
    case _ => formatter.indentedElement("li", item)

  private def confirmTaskMarkers(block: Block): Block = block match
    case list: BulletList =>
      list.copy(content = list.content.map(confirmTaskMarkers))
    case list: EnumList =>
      list.copy(content = list.content.map(confirmTaskMarkers))
    case other => other

  private def confirmTaskMarkers(item: BulletListItem): BulletListItem =
    item.copy(content = confirmTaskMarkers(item.content))

  private def confirmTaskMarkers(item: EnumListItem): EnumListItem =
    item.copy(content = confirmTaskMarkers(item.content))

  private def confirmTaskMarkers(blocks: Seq[Block]): Seq[Block] =
    val nested = blocks.map(confirmTaskMarkers)
    nested match
      case head +: tail => confirmTaskMarker(head) +: tail
      case _            => nested

  private def confirmTaskMarker(block: Block): Block = block match
    case paragraph: Paragraph =>
      paragraph.withContent(confirmTaskMarker(paragraph.content))
    case paragraph: ForcedParagraph =>
      paragraph.withContent(confirmTaskMarker(paragraph.content))
    case spans: SpanSequence =>
      spans.withContent(confirmTaskMarker(spans.content))
    case other => other

  private def confirmTaskMarker(spans: Seq[Span]): Seq[Span] = spans match
    case (candidate: TaskListCandidate) +: tail =>
      TaskListMarker(
        candidate.checked,
        candidate.marker,
        candidate.source,
        candidate.options,
      ) +: tail
    case _ => spans

  private def rewrite(list: BulletList): BulletList =
    val content   = list.content.map(rewrite)
    val rewritten = list.copy(content = content)
    if containsTask(content) then rewritten.withStyle(ContainsTaskList)
    else rewritten

  private def rewrite(list: EnumList): EnumList =
    val content   = list.content.map(rewrite)
    val rewritten = list.copy(content = content)
    if containsTask(content) then rewritten.withStyle(ContainsTaskList)
    else rewritten

  private def rewrite(item: BulletListItem): BulletListItem =
    item.content match
      case head +: tail =>
        rewriteFirstBlock(head)
          .fold(item)(newHead =>
            item.copy(
              content = newHead +: tail,
              options = item.options + Styles(TaskItemStyle),
            ),
          )
      case _ => item

  private def rewrite(item: EnumListItem): EnumListItem =
    item.content match
      case head +: tail =>
        rewriteFirstBlock(head)
          .fold(item)(newHead =>
            item.copy(
              content = newHead +: tail,
              options = item.options + Styles(TaskItemStyle),
            ),
          )
      case _ => item

  private def rewriteFirstBlock(block: Block): Option[Block] = block match
    case paragraph: Paragraph =>
      rewriteMarker(paragraph.content).map(paragraph.withContent)
    case paragraph: ForcedParagraph =>
      rewriteMarker(paragraph.content).map(paragraph.withContent)
    case spans: SpanSequence =>
      rewriteMarker(spans.content).map(spans.withContent)
    case _ => None

  private def rewriteMarker(spans: Seq[Span]): Option[Seq[Span]] = spans match
    case (marker: TaskListMarker) +: tail =>
      Some(TaskListCheckbox(marker.checked, marker.options) +: tail)
    case _ => None

  private def containsTask(items: Seq[ListItem]): Boolean =
    items.exists(_.hasStyle(TaskItemStyle)) || items.exists:
      case item: BlockContainer => containsNestedTaskList(item.content)
      case _                    => false

  private def containsNestedTaskList(blocks: Seq[Block]): Boolean =
    blocks.exists:
      case list: BulletList => list.hasStyle(ContainsTaskList)
      case list: EnumList   => list.hasStyle(ContainsTaskList)
      case _                => false

private[sbtplugin] final case class TaskListCheckbox(
    checked: Boolean,
    options: Options = Options.empty,
) extends Span
    with Fallback:
  type Self = TaskListCheckbox

  def fallback: Element = Text(if checked then "[x]" else "[ ]", options)

  def withOptions(options: Options): TaskListCheckbox = copy(options = options)

private[sbtplugin] final case class TaskListCandidate(
    checked: Boolean,
    marker: String,
    source: SourceFragment,
    options: Options = Options.empty,
) extends Span
    with Fallback:
  type Self = TaskListCandidate

  def fallback: Element = Text(marker, options)

  def withOptions(options: Options): TaskListCandidate = copy(options = options)

private[sbtplugin] final case class TaskListMarker(
    checked: Boolean,
    marker: String,
    source: SourceFragment,
    options: Options = Options.empty,
) extends Span
    with Fallback:
  type Self = TaskListMarker

  def fallback: Element = Text(marker, options)

  def withOptions(options: Options): TaskListMarker = copy(options = options)
