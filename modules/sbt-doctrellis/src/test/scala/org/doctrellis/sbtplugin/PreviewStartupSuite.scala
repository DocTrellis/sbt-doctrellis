package org.doctrellis.sbtplugin

import java.io.IOException
import java.net.BindException
import java.util.concurrent.{CountDownLatch, Executors, TimeUnit}

import sbt.util.LogExchange

class PreviewStartupSuite extends munit.FunSuite:
  test("an HTTP bind failure keeps the specific port diagnostic"):
    val failure = BindException("Address already in use")

    val actual = intercept[IllegalStateException]:
      PreviewExecution.run("127.0.0.1", 4242):
        PreviewStartup.startServing(
          initialBuild = () => 1L,
          ensureWatcherHealthy = () => (),
          startServing = () => throw failure,
        )

    assertEquals(
      actual.getMessage,
      "DocTrellis preview could not bind to 127.0.0.1:4242; the port may already be in use: Address already in use",
    )
    assertEquals(actual.getCause, failure)

  test("an I/O failure outside HTTP binding uses a preview-wide diagnostic"):
    val failure = IOException("watch service failed")

    val actual = intercept[IllegalStateException]:
      PreviewExecution.run("127.0.0.1", 4242):
        throw failure

    assertEquals(
      actual.getMessage,
      "DocTrellis preview failed while starting or running: watch service failed",
    )
    assertEquals(actual.getCause, failure)

  test("a pre-bind BindException is not reported as port contention"):
    val failure = BindException("watcher health failed")

    val actual = intercept[IllegalStateException]:
      PreviewExecution.run("127.0.0.1", 4242):
        PreviewStartup.startServing(
          initialBuild = () => 1L,
          ensureWatcherHealthy = () => throw failure,
          startServing = () => (),
        )

    assertEquals(
      actual.getMessage,
      "DocTrellis preview failed while starting or running: watcher health failed",
    )
    assertEquals(actual.getCause, failure)

  test("an initial build failure prevents the serving boundary"):
    val failure        = IllegalStateException("initial build failed")
    var servingStarted = false

    val actual = intercept[IllegalStateException]:
      PreviewStartup.startServing(
        initialBuild = () => throw failure,
        ensureWatcherHealthy = () => (),
        startServing = () =>
          servingStarted = true
          (),
      )

    assertEquals(actual, failure)
    assert(!servingStarted)

  test("an invalidation queued during the initial build reaches the loop"):
    val mailbox  = BlockingPreviewEventMailbox()
    var rebuilds = 0

    PreviewStartup.startServing(
      initialBuild = () =>
        mailbox.invalidate()
        1L
      ,
      ensureWatcherHealthy = () => mailbox.throwIfWatcherFailed(),
      startServing = () => (),
    )
    PreviewRebuildLoop.run(
      mailbox = mailbox,
      quietPeriodNanos = 0L,
      rebuild = () =>
        rebuilds += 1
        mailbox.stop()
        2L
      ,
      onPublished = _ => (),
      log = LogExchange.logger("preview-startup-test"),
    )

    assertEquals(rebuilds, 1)

  test("JVM shutdown waiting completes only after main cleanup signals"):
    val completion = PreviewCleanupCompletion()
    val entered    = CountDownLatch(1)
    val executor   = Executors.newSingleThreadExecutor()
    try
      val waiting = executor.submit(() =>
        entered.countDown()
        completion.await(TestTimeout.Seconds, TimeUnit.SECONDS),
      )
      assert(entered.await(TestTimeout.Seconds, TimeUnit.SECONDS))
      assert(!waiting.isDone)
      completion.complete()
      completion.complete()
      assert(waiting.get(TestTimeout.Seconds, TimeUnit.SECONDS))
    finally executor.shutdownNow(): Unit
