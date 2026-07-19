package org.doctrellis.vscode.domain

import hedgehog.*
import hedgehog.munit.HedgehogSuite

class ModelPropertySuite extends HedgehogSuite:
  property("accepted executable names preserve their representation"):
    for executable <- genExecutable.forAll
        .cover(10, "contains whitespace", _.exists(_.isWhitespace))
        .cover(10, "contains non-ASCII", _.exists(_ > '\u007f'))
    yield Executable.parse(executable).map(_.value) ==== Right(executable)

  property("blank executable names are rejected"):
    for executable <- genBlankExecutable.forAll
    yield Result.assert(Executable.parse(executable).isLeft)

  property("executable names containing NUL are rejected"):
    for executable <- genNulExecutable.forAll
    yield Result.assert(Executable.parse(executable).isLeft)

  property("startup timeouts accept exactly the documented range"):
    for milliseconds <- genStartupTimeout.forAll
    yield
      val parsed  = StartupTimeout.parse(milliseconds)
      val inRange = milliseconds >= 1 && milliseconds <= 600000
      Result.all(
        List(
          parsed.isRight ==== inRange,
          parsed.toOption.map(_.duration.toMillis) ====
            Option.when(inRange)(milliseconds.toLong),
        ),
      )

  property("poll intervals accept exactly the documented range"):
    for milliseconds <- genPollInterval.forAll
    yield
      val parsed  = PollInterval.parse(milliseconds)
      val inRange = milliseconds >= 100 && milliseconds <= 10000
      Result.all(
        List(
          parsed.isRight ==== inRange,
          parsed.toOption.map(_.duration.toMillis) ====
            Option.when(inRange)(milliseconds.toLong),
        ),
      )

  property("HTTP URL ports accept exactly the valid port range"):
    for port <- genPort.forAll
    yield
      val url     = s"http://localhost:$port"
      val parsed  = PreviewUrl.parse(url)
      val inRange = port >= 1 && port <= 65535
      Result.all(
        List(
          parsed.isRight ==== inRange,
          parsed.toOption.map(_.value) ==== Option.when(inRange)(url),
        ),
      )

  private def genExecutable: Gen[String] =
    Gen.choice1(
      genLowerExecutable,
      genSpacedExecutable,
      genUnicodeExecutable,
    )

  private def genSpacedExecutable: Gen[String] =
    for
      prefix <- Gen.string(Gen.lower, Range.linear(1, 31))
      suffix <- Gen.string(Gen.lower, Range.linear(1, 31))
    yield s"$prefix $suffix"

  private def genUnicodeExecutable: Gen[String] =
    for
      prefix  <- Gen.string(Gen.lower, Range.linear(1, 31))
      unicode <- Gen.unicode.map: value =>
        if value <= '\u007f' then '\ud55c' else value
      suffix <- Gen.string(Gen.lower, Range.linear(0, 31))
    yield s"$prefix$unicode$suffix"

  private def genLowerExecutable: Gen[String] =
    Gen.string(Gen.lower, Range.linear(1, 64))

  private def genBlankExecutable: Gen[String] =
    Gen.string(
      Gen.choice1(
        Gen.constant(' '),
        Gen.constant('\t'),
        Gen.constant('\r'),
        Gen.constant('\n'),
      ),
      Range.linear(0, 64),
    )

  private def genNulExecutable: Gen[String] =
    for
      prefix <- Gen.string(Gen.unicode, Range.linear(0, 32))
      suffix <- Gen.string(Gen.unicode, Range.linear(0, 32))
    yield prefix + '\u0000' + suffix

  private def genStartupTimeout: Gen[Int] =
    Gen.choice1(
      Gen.int(Range.linearFrom(0, -1000000, 0)),
      Gen.constant(1),
      Gen.int(Range.linear(2, 599999)),
      Gen.constant(600000),
      Gen.int(Range.linear(600001, 1000000)),
    )

  private def genPollInterval: Gen[Int] =
    Gen.choice1(
      Gen.int(Range.linearFrom(0, -100000, 99)),
      Gen.constant(100),
      Gen.int(Range.linear(101, 9999)),
      Gen.constant(10000),
      Gen.int(Range.linear(10001, 100000)),
    )

  private def genPort: Gen[Int] =
    Gen.choice1(
      Gen.constant(0),
      Gen.constant(1),
      Gen.int(Range.linear(2, 65534)),
      Gen.constant(65535),
      Gen.int(Range.linear(65536, 99999)),
    )
