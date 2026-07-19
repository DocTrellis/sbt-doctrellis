package org.doctrellis.sbtplugin

import munit.FunSuite

class PreviewServerShutdownSuite extends FunSuite:
  test("shutdown is idempotent"):
    var serverStops   = 0
    var executorStops = 0
    var signals       = 0
    val shutdown      = PreviewServerShutdown(
      stopServer = () => serverStops += 1,
      stopExecutor = () => executorStops += 1,
      signalStopped = () => signals += 1,
    )

    shutdown.stop()
    shutdown.stop()

    assertEquals(serverStops, 1)
    assertEquals(executorStops, 1)
    assertEquals(signals, 1)

  test("shutdown attempts every action after an earlier failure"):
    val failure       = IllegalStateException("server stop failed")
    var executorStops = 0
    var signals       = 0
    val shutdown      = PreviewServerShutdown(
      stopServer = () => throw failure,
      stopExecutor = () => executorStops += 1,
      signalStopped = () => signals += 1,
    )

    val actual = intercept[IllegalStateException](shutdown.stop())
    shutdown.stop()

    assertEquals(actual, failure)
    assertEquals(executorStops, 1)
    assertEquals(signals, 1)

  test("shutdown preserves a primary failure"):
    val primary  = IllegalStateException("server start failed")
    val cleanup  = IllegalStateException("server stop failed")
    val shutdown = PreviewServerShutdown(
      stopServer = () => throw cleanup,
      stopExecutor = () => (),
      signalStopped = () => (),
    )

    shutdown.stopPreserving(Some(primary))

    assertEquals(primary.getSuppressed.toVector, Vector(cleanup))
