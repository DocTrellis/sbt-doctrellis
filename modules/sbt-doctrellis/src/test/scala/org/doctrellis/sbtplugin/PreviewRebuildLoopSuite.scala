package org.doctrellis.sbtplugin

import java.nio.file.{Files, Path}
import java.util.concurrent.{Callable, CountDownLatch, Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.Using

import sbt.util.LogExchange

class PreviewRebuildLoopSuite extends munit.FunSuite:
  test("timestamp capture is serialized with the after-build empty drain"):
    val timestampCaptured = CountDownLatch(1)
    val releaseEnqueue    = CountDownLatch(1)
    val drainStarted      = CountDownLatch(1)
    val drainCompleted    = CountDownLatch(1)
    val drained           = AtomicReference[Option[PreviewLoopEvent]]()
    val pool              = Executors.newFixedThreadPool(2)
    val mailbox           = BlockingPreviewEventMailbox(() =>
      timestampCaptured.countDown()
      if !releaseEnqueue.await(TestTimeout.Seconds, TimeUnit.SECONDS) then
        throw AssertionError("test did not release the invalidation enqueue")
      7L,
    )

    try
      val invalidation = pool.submit(
        new Callable[Unit]:
          override def call(): Unit = mailbox.invalidate(),
      )
      assert(timestampCaptured.await(TestTimeout.Seconds, TimeUnit.SECONDS))
      val drain = pool.submit(
        new Callable[Unit]:
          override def call(): Unit =
            drainStarted.countDown()
            drained.set(mailbox.pollNow())
            drainCompleted.countDown(),
      )
      assert(drainStarted.await(TestTimeout.Seconds, TimeUnit.SECONDS))
      assert(
        !drainCompleted.await(100L, TimeUnit.MILLISECONDS),
        "the drain must not pass a timestamped invalidation before its enqueue",
      )

      releaseEnqueue.countDown()
      invalidation.get(TestTimeout.Seconds, TimeUnit.SECONDS)
      drain.get(TestTimeout.Seconds, TimeUnit.SECONDS)
      assertEquals(
        drained.get(),
        Some(PreviewLoopEvent.Invalidated(7L)),
      )
    finally
      releaseEnqueue.countDown()
      pool.shutdownNow(): Unit

  test("coalesces invalidations and runs one pending build after a build"):
    val mailbox = ScriptedMailbox(
      List(
        Step.Event(PreviewLoopEvent.Invalidated(1L)),
        Step.Event(PreviewLoopEvent.Invalidated(2L)),
        Step.Quiet,
      ),
    )
    var builds    = 0
    var building  = false
    val published = ArrayBuffer.empty[Long]

    PreviewRebuildLoop.run(
      mailbox = mailbox,
      quietPeriodNanos = 10L,
      rebuild = () =>
        assert(!building, "builds must never overlap")
        building = true
        builds += 1
        if builds == 1 then
          mailbox.append(
            Step.Event(PreviewLoopEvent.Invalidated(3L)),
            Step.Event(PreviewLoopEvent.Invalidated(4L)),
            Step.Quiet,
          )
        else mailbox.append(Step.Event(PreviewLoopEvent.Stop))
        building = false
        builds.toLong
      ,
      onPublished = published += _,
      log = logger,
      clockNanos = () => 10L,
    )

    assertEquals(builds, 2)
    assertEquals(published.toVector, Vector(1L, 2L))
    assertEquals(mailbox.pollBeforeCalls, 2)

  test(
    "shutdown queued during a rebuild publishes before loop cleanup",
  ):
    withLayout: layout =>
      val publication = PreviewPublication.open(layout, logger)
      assertEquals(
        publication.buildAndPublish(target =>
          writeGeneration(target, "initial"),
        ),
        1L,
      )
      val staged          = publication.stagedTarget
      val mailbox         = BlockingPreviewEventMailbox()
      val buildStarted    = CountDownLatch(1)
      val releaseBuild    = CountDownLatch(1)
      val cleanupFinished = CountDownLatch(1)
      val serverStopped   = AtomicBoolean(false)
      val executorStopped = AtomicBoolean(false)
      val published       = ArrayBuffer.empty[Long]
      val worker          = Executors.newSingleThreadExecutor()
      mailbox.invalidate()
      val loop = worker.submit(
        new Callable[Unit]:
          override def call(): Unit =
            try
              PreviewRebuildLoop.run(
                mailbox = mailbox,
                quietPeriodNanos = 0L,
                rebuild = () =>
                  publication.buildAndPublish: target =>
                    Files.createDirectories(target.mdocDirectory)
                    Files.createDirectories(target.siteDirectory)
                    Files.writeString(
                      target.mdocDirectory.resolve("index.md"),
                      "rebuilt",
                    )
                    buildStarted.countDown()
                    if !releaseBuild.await(
                        TestTimeout.Seconds,
                        TimeUnit.SECONDS,
                      )
                    then
                      throw AssertionError(
                        "test did not release the staged rebuild",
                      )
                    Files.writeString(
                      target.siteDirectory.resolve("index.html"),
                      "rebuilt",
                    ): Unit
                ,
                onPublished = published += _,
                log = logger,
              )
            finally
              try publication.close()
              finally cleanupFinished.countDown(),
      )
      val shutdown = PreviewServerShutdown(
        stopServer = () => serverStopped.set(true),
        stopExecutor = () => executorStopped.set(true),
        signalStopped = () => mailbox.stop(),
      )

      try
        assert(buildStarted.await(TestTimeout.Seconds, TimeUnit.SECONDS))
        assert(
          Files.isRegularFile(staged.mdocDirectory.resolve("index.md")),
        )
        assert(!Files.exists(staged.siteDirectory.resolve("index.html")))
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "initial",
        )
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "initial",
        )

        shutdown.stop()
        assert(serverStopped.get())
        assert(executorStopped.get())
        assert(!loop.isDone)
        assert(
          !cleanupFinished.await(200L, TimeUnit.MILLISECONDS),
          "publication cleanup must wait for the in-progress rebuild",
        )

        releaseBuild.countDown()
        loop.get(TestTimeout.Seconds, TimeUnit.SECONDS)
        assert(cleanupFinished.await(TestTimeout.Seconds, TimeUnit.SECONDS))
        assertEquals(published.toVector, Vector(2L))
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "rebuilt",
        )
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "rebuilt",
        )
        assertEquals(ownedTemporaryPaths(layout), Vector.empty)
      finally
        releaseBuild.countDown()
        mailbox.stop()
        if !loop.isDone then
          try loop.get(TestTimeout.Seconds, TimeUnit.SECONDS): Unit
          catch case _: Exception => ()
        worker.shutdownNow(): Unit

  test("events first observed after a build still receive a quiet period"):
    val mailbox = ScriptedMailbox(
      List(
        Step.Event(PreviewLoopEvent.Invalidated(1L)),
        Step.Quiet,
      ),
    )
    var builds                  = 0
    var secondBuildWasDebounced = false

    PreviewRebuildLoop.run(
      mailbox = mailbox,
      quietPeriodNanos = 10L,
      rebuild = () =>
        builds += 1
        if builds == 1 then
          mailbox.append(
            Step.Event(PreviewLoopEvent.Invalidated(20L)),
            Step.Quiet,
            Step.Quiet,
          )
        else
          secondBuildWasDebounced = mailbox.pollBeforeCalls == 2
          mailbox.append(Step.Event(PreviewLoopEvent.Stop))
        builds.toLong
      ,
      onPublished = _ => (),
      log = logger,
      clockNanos = () => 10L,
    )

    assertEquals(builds, 2)
    assert(secondBuildWasDebounced)

  test("a later build failure is non-terminal and the next change retries"):
    val mailbox = ScriptedMailbox(
      List(
        Step.Event(PreviewLoopEvent.Invalidated(1L)),
        Step.Quiet,
        Step.Event(PreviewLoopEvent.Invalidated(2L)),
        Step.Quiet,
        Step.Event(PreviewLoopEvent.Stop),
      ),
    )
    var attempts  = 0
    val published = ArrayBuffer.empty[Long]

    PreviewRebuildLoop.run(
      mailbox = mailbox,
      quietPeriodNanos = 10L,
      rebuild = () =>
        attempts += 1
        if attempts == 1 then throw IllegalStateException("invalid document")
        2L
      ,
      onPublished = published += _,
      log = logger,
    )

    assertEquals(attempts, 2)
    assertEquals(published.toVector, Vector(2L))

  test("a watcher failure is terminal and does not run another build"):
    val failure = IllegalStateException("watch service failed")
    val mailbox = ScriptedMailbox(
      List(Step.Event(PreviewLoopEvent.WatcherFailed(failure))),
    )
    var builds = 0

    val actual = intercept[IllegalStateException](
      PreviewRebuildLoop.run(
        mailbox = mailbox,
        quietPeriodNanos = 10L,
        rebuild = () =>
          builds += 1
          1L
        ,
        onPublished = _ => (),
        log = logger,
      ),
    )

    assertEquals(actual, failure)
    assertEquals(builds, 0)

  test("stop during the quiet period prevents a rebuild"):
    val mailbox = ScriptedMailbox(
      List(
        Step.Event(PreviewLoopEvent.Invalidated(1L)),
        Step.Event(PreviewLoopEvent.Stop),
      ),
    )
    var builds = 0

    PreviewRebuildLoop.run(
      mailbox = mailbox,
      quietPeriodNanos = 10L,
      rebuild = () =>
        builds += 1
        1L
      ,
      onPublished = _ => (),
      log = logger,
    )

    assertEquals(builds, 0)

  private enum Step:
    case Event(value: PreviewLoopEvent)
    case Quiet

  private final class ScriptedMailbox(initial: List[Step])
      extends PreviewEventMailbox:
    private var remaining = initial
    var pollBeforeCalls   = 0

    def append(steps: Step*): Unit = remaining = remaining ++ steps

    override def take(): PreviewLoopEvent = next() match
      case Step.Event(value) => value
      case Step.Quiet        => fail("take encountered a quiet-period boundary")

    override def pollNow(): Option[PreviewLoopEvent] = remaining match
      case Step.Event(value) :: tail =>
        remaining = tail
        Some(value)
      case Step.Quiet :: tail =>
        remaining = tail
        None
      case Nil => None

    override def pollBefore(
        deadlineNanos: Long,
    ): Option[PreviewLoopEvent] =
      val _ = deadlineNanos
      pollBeforeCalls += 1
      next() match
        case Step.Event(value) => Some(value)
        case Step.Quiet        => None

    private def next(): Step = remaining match
      case head :: tail =>
        remaining = tail
        head
      case Nil => fail("scripted mailbox ran out of steps")

  private def logger = LogExchange.logger("preview-rebuild-loop-test")

  private def writeGeneration(
      target: DocTrellisBuildTarget,
      content: String,
  ): Unit =
    Files.createDirectories(target.mdocDirectory)
    Files.createDirectories(target.siteDirectory)
    Files.writeString(target.mdocDirectory.resolve("index.md"), content)
    Files.writeString(target.siteDirectory.resolve("index.html"), content): Unit

  private def ownedTemporaryPaths(layout: DocTrellisLayout): Vector[Path] =
    Vector(
      layout.mdocDirectory.getParent,
      layout.siteDirectory.getParent,
    ).distinct
      .flatMap: parent =>
        Using.resource(Files.list(parent)): paths =>
          paths
            .iterator()
            .asScala
            .toVector
            .filter: path =>
              val name = path.getFileName.toString
              name.endsWith(".owner") || name.endsWith(".staging") ||
              name.endsWith(".backup") || name.endsWith(".commit") ||
              name.endsWith(".owner.tmp") || name.endsWith(".commit.tmp")

  private def withLayout(action: DocTrellisLayout => Unit): Unit =
    val root      = Files.createTempDirectory("doctrellis-rebuild-shutdown-")
    val workspace = Files.createDirectories(root.resolve("workspace"))
    val source    = Files.createDirectories(workspace.resolve("docs"))
    val output    = Files.createDirectories(root.resolve("output"))
    val layout    = DocTrellisLayout
      .parse(
        workspaceDirectory = workspace,
        sourceDirectory = source,
        mdocDirectory = output.resolve("mdoc"),
        siteDirectory = output.resolve("site"),
      )
      .fold(problem => fail(problem.message), identity)
    try action(layout)
    finally GeneratedDirectory.delete(root)
