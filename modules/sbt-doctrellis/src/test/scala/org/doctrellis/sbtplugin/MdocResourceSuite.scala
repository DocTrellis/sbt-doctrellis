package org.doctrellis.sbtplugin

class MdocResourceSuite extends munit.FunSuite:
  test("releases nested resources and preserves the primary failure"):
    val operationFailure = IllegalStateException("operation failed")
    val engineFailure    = IllegalStateException("engine cleanup failed")
    val loaderFailure    = IllegalStateException("loader cleanup failed")
    var releasedEngine   = false
    var releasedLoader   = false

    val actual = intercept[IllegalStateException]:
      MdocResource.use("loader")(_ =>
        releasedLoader = true
        throw loaderFailure,
      ): _ =>
        MdocResource.use("engine")(_ =>
          releasedEngine = true
          throw engineFailure,
        ): _ =>
          throw operationFailure

    assertEquals(actual, operationFailure)
    assert(releasedEngine)
    assert(releasedLoader)
    assertEquals(
      actual.getSuppressed.toVector,
      Vector(engineFailure, loaderFailure),
    )

  test("releases the outer resource when inner acquisition fails"):
    val acquisitionFailure = IllegalStateException("engine acquisition failed")
    val cleanupFailure     = IllegalStateException("loader cleanup failed")
    var releasedLoader     = false

    val actual = intercept[IllegalStateException]:
      MdocResource.use("loader")(_ =>
        releasedLoader = true
        throw cleanupFailure,
      ): _ =>
        MdocResource.use[String, Unit](throw acquisitionFailure)(_ => ())(_ =>
          (),
        )

    assertEquals(actual, acquisitionFailure)
    assert(releasedLoader)
    assertEquals(actual.getSuppressed.toVector, Vector(cleanupFailure))
