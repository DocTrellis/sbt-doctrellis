package org.doctrellis.sbtplugin

import hedgehog.*
import hedgehog.munit.HedgehogSuite

class GfmListIndentationPropertySuite extends HedgehogSuite:
  property("valid task-list indentation normalization is idempotent"):
    for markdown <- genTaskListDocument.forAll
    yield
      val once  = GfmListIndentation.normalize(markdown)
      val twice = GfmListIndentation.normalize(once)
      Result.all(
        List(
          twice ==== once,
          lineEndings(once) ==== lineEndings(markdown),
        ),
      )

  property("documents without task-list items remain byte-for-byte unchanged"):
    for markdown <- genTaskFreeDocument.forAll
    yield GfmListIndentation.normalize(markdown) ==== markdown

  property("normalization changes only leading spaces"):
    for markdown <- genFreeformDocument.forAll.cover(
        10,
        "normalization changed input",
        input => GfmListIndentation.normalize(input) != input,
      )
    yield
      val normalized = GfmListIndentation.normalize(markdown)
      Result.all(
        List(
          lineEndings(normalized) ==== lineEndings(markdown),
          lineBodies(normalized) ==== lineBodies(markdown),
        ),
      )

  private def genLineEnding: Gen[String] =
    Gen.choice1(
      Gen.constant("\n"),
      Gen.constant("\r\n"),
      Gen.constant("\r"),
    )

  private def genListMarker: Gen[String] =
    Gen.choice1(
      Gen.constant("-"),
      Gen.constant("+"),
      Gen.constant("*"),
      Gen.constant("1."),
    )

  private def genTaskMarker: Gen[String] =
    Gen.choice1(
      Gen.constant("[ ]"),
      Gen.constant("[x]"),
      Gen.constant("[X]"),
    )

  private def genText: Gen[String] =
    Gen.string(Gen.lower, Range.linear(1, 40))

  private def genTaskListDocument: Gen[String] =
    genTaskListDocument(Gen.int(Range.linear(2, 5)))

  private def genEditedTaskListDocument: Gen[String] =
    genTaskListDocument(Gen.element1(3, 5))

  private def genTaskListDocument(indentation: Gen[Int]): Gen[String] =
    for
      ending       <- genLineEnding
      parentMarker <- genListMarker
      parentTask   <- genTaskMarker
      parentText   <- genText
      childIndent  <- indentation
      childMarker  <- genListMarker
      childTask    <- genTaskMarker
      childText    <- genText
    yield s"$parentMarker $parentTask $parentText$ending" +
      s"${" " * childIndent}$childMarker $childTask $childText$ending"

  private def genTaskFreeDocument: Gen[String] =
    for
      ending     <- genLineEnding
      paragraph  <- genText
      parentText <- genText
      childText  <- genText
      fencedText <- genText
    yield s"$paragraph$ending$ending" +
      s"- $parentText$ending" +
      s"  - $childText$ending$ending" +
      s"```text$ending" +
      s"- [x] $fencedText$ending" +
      s"```$ending"

  private def genFreeformDocument: Gen[String] =
    Gen.frequency1(
      1 -> genEditedTaskListDocument,
      3 -> genArbitraryDocument,
    )

  private def genArbitraryDocument: Gen[String] =
    Gen
      .choice1(
        Gen.string(Gen.unicode, Range.linear(0, 32)),
        Gen.constant("\n"),
        Gen.constant("\r\n"),
        Gen.constant("\r"),
        Gen.constant("  "),
        Gen.constant("\t"),
        Gen.constant("- [ ] "),
        Gen.constant("1. [x] "),
        Gen.constant("```"),
        Gen.constant("~~~"),
      )
      .list(Range.linear(0, 64))
      .map(_.mkString)

  private def lineEndings(input: String): List[String] =
    "\r\n|\r|\n".r.findAllIn(input).toList

  private def lineBodies(input: String): List[String] =
    input
      .split("\\r\\n|\\r|\\n", -1)
      .toList
      .map(_.dropWhile(_ == ' '))
