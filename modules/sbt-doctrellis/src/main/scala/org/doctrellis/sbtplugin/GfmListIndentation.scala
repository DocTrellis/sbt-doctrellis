package org.doctrellis.sbtplugin

import scala.collection.mutable

/** Adapts GFM list indentation to Laika 1.3.2 without changing task marker
  * syntax. Laika requires four spaces for every nested list level, while GFM
  * uses the parent marker's content column (two spaces for a typical bullet,
  * three for `1.`). The public list parser cannot be replaced or configured, so
  * this line-state preprocessor is limited to list regions that contain a task
  * marker. It preserves original line endings and never scans fenced code as
  * list content. Diagnostics on normalized nested lines therefore report the
  * adjusted Laika input column instead of the original Markdown column.
  */
private[sbtplugin] object GfmListIndentation:

  private val ListItemLine =
    """^( *)([-+*]|[0-9]{1,9}\.)([ \t]+)(.*)$""".r
  private val FenceLine = """^( *)(`{3,}|~{3,})(.*)$""".r

  private final case class SourceLine(content: String, ending: String)

  private final case class ListItemContext(
      contentIndent: Int,
      normalizedMarkerIndent: Int,
  )

  private final case class IndentEdit(
      line: Int,
      original: Int,
      normalized: Int,
      segment: Int,
  )

  private final case class Fence(
      marker: Char,
      length: Int,
      containerIndent: Int,
  )

  def normalize(input: String): String =
    val lines        = splitLines(input)
    val edits        = mutable.ArrayBuffer.empty[IndentEdit]
    val taskSegments = mutable.HashSet.empty[Int]
    val stack        = mutable.ArrayBuffer.empty[ListItemContext]

    var nextSegment   = 0
    var activeSegment = Option.empty[Int]
    var fence         = Option.empty[Fence]

    def segment: Int = activeSegment.getOrElse:
      val current = nextSegment
      nextSegment += 1
      activeSegment = Some(current)
      current

    def endList(): Unit =
      stack.clear()
      activeSegment = None

    def containingItem(indentation: Int): Option[ListItemContext] =
      while stack.lastOption.exists(indentation < _.contentIndent) do
        stack.remove(stack.length - 1): Unit
      stack.lastOption.filter(indentation >= _.contentIndent)

    def editContinuation(line: SourceLine, index: Int): Unit =
      stack.lastOption.foreach: current =>
        val indentation = leadingSpaces(line.content)
        if indentation >= current.contentIndent then
          edits += IndentEdit(
            index,
            indentation,
            current.normalizedMarkerIndent + 4 +
              (indentation - current.contentIndent),
            segment,
          )

    lines.zipWithIndex.foreach: (line, index) =>
      fence match
        case Some(open) =>
          editContinuation(line, index)
          if closesFence(line.content, open) then fence = None
        case None =>
          line.content match
            case ListItemLine(indentation, marker, whitespace, content) =>
              val originalIndent = indentation.length
              val parent         = containingItem(originalIndent)
              val isListItem     = parent
                .fold(originalIndent <= 3)(item =>
                  originalIndent <= item.contentIndent + 3,
                )

              if isListItem then
                val normalizedIndent = parent
                  .map(_.normalizedMarkerIndent + 4)
                  .getOrElse(originalIndent)
                val currentSegment = segment
                edits += IndentEdit(
                  index,
                  originalIndent,
                  normalizedIndent,
                  currentSegment,
                )
                if isTaskMarker(content) then taskSegments += currentSegment

                stack += ListItemContext(
                  contentIndent(
                    originalIndent,
                    marker.length,
                    whitespace,
                  ),
                  normalizedIndent,
                )
              else if parent.nonEmpty then editContinuation(line, index)
              else endList()

            case FenceLine(indentation, marker, _) =>
              val originalIndent  = indentation.length
              val parent          = containingItem(originalIndent)
              val containerIndent = parent.fold(0)(_.contentIndent)
              val relativeIndent  = originalIndent - containerIndent
              parent.fold(endList())(_ => editContinuation(line, index))
              if relativeIndent >= 0 && relativeIndent <= 3 then
                val markerCharacter =
                  if marker.startsWith("`") then '`' else '~'
                fence = Some(
                  Fence(
                    markerCharacter,
                    marker.length,
                    containerIndent,
                  ),
                )

            case _ if line.content.trim.isEmpty => ()
            case _                              =>
              val indentation = leadingSpaces(line.content)
              if containingItem(indentation).nonEmpty then
                editContinuation(line, index)
              else endList()

    if taskSegments.isEmpty then input
    else
      val editsByLine = edits.iterator
        .filter(edit => taskSegments.contains(edit.segment))
        .filter(edit => edit.original != edit.normalized)
        .map(edit => edit.line -> edit)
        .toMap

      lines.zipWithIndex
        .map: (line, index) =>
          editsByLine
            .get(index)
            .fold(line.content + line.ending): edit =>
              (" " * edit.normalized) +
                line.content.drop(edit.original) +
                line.ending
        .mkString

  private def contentIndent(
      markerIndent: Int,
      markerWidth: Int,
      whitespace: String,
  ): Int =
    val markerEnd = markerIndent + markerWidth
    val padding   = advanceColumn(markerEnd, whitespace) - markerEnd
    markerEnd + (if padding <= 4 then padding else 1)

  private def advanceColumn(start: Int, whitespace: String): Int =
    whitespace.foldLeft(start): (column, char) =>
      if char == '\t' then column + (4 - (column % 4)) else column + 1

  private def isTaskMarker(content: String): Boolean =
    val marker = content.take(3)
    val valid  = marker == "[ ]" || marker == "[x]" || marker == "[X]"
    valid && (content.length == 3 || content.charAt(3).isWhitespace)

  private def leadingSpaces(input: String): Int =
    input.takeWhile(_ == ' ').length

  private def closesFence(input: String, fence: Fence): Boolean =
    val indentation    = leadingSpaces(input)
    val relativeIndent = indentation - fence.containerIndent
    val content        = input.drop(indentation)
    val marker         = content.takeWhile(_ == fence.marker)
    relativeIndent >= 0 && relativeIndent <= 3 &&
    marker.length >= fence.length && content.drop(marker.length).trim.isEmpty

  private def splitLines(input: String): Vector[SourceLine] =
    val result = mutable.ArrayBuffer.empty[SourceLine]
    var start  = 0
    var index  = 0

    while index < input.length do
      input.charAt(index) match
        case '\r' =>
          val hasLineFeed =
            index + 1 < input.length && input.charAt(index + 1) == '\n'
          val ending = if hasLineFeed then "\r\n" else "\r"
          result += SourceLine(input.substring(start, index), ending)
          index += (if hasLineFeed then 2 else 1)
          start = index
        case '\n' =>
          result += SourceLine(input.substring(start, index), "\n")
          index += 1
          start = index
        case _ => index += 1

    if start < input.length || result.isEmpty then
      result += SourceLine(input.substring(start), "")
    result.toVector
