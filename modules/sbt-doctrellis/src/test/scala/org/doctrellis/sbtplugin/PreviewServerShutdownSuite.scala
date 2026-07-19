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

  test("shutdown preserves secondary cleanup failures"):
    val serverFailure   = IllegalStateException("server stop failed")
    val executorFailure = IllegalStateException("executor stop failed")
    val shutdown        = PreviewServerShutdown(
      stopServer = () => throw serverFailure,
      stopExecutor = () => throw executorFailure,
      signalStopped = () => (),
    )

    val actual = intercept[IllegalStateException](shutdown.stop())

    assertEquals(actual, serverFailure)
    assertEquals(actual.getSuppressed.toVector, Vector(executorFailure))

  test("IPC previews require the loopback host while direct previews do not"):
    PreviewHost.requireCompatible("0.0.0.0", controlEnabled = false)
    PreviewHost.requireCompatible("127.0.0.1", controlEnabled = true)
    val failure = intercept[IllegalArgumentException](
      PreviewHost.requireCompatible("0.0.0.0", controlEnabled = true),
    )
    assert(failure.getMessage.contains("127.0.0.1"))
