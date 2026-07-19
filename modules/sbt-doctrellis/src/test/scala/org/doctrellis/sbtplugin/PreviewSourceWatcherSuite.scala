package org.doctrellis.sbtplugin

import java.nio.file.{Files, Path, StandardWatchEventKinds}
import java.util.concurrent.{Callable, Executors, TimeUnit}

class PreviewSourceWatcherSuite extends munit.FunSuite:
  test("parent events filter siblings while overflow invalidates"):
    val parent = Path.of("parent")
    val source = Path.of("docs")
    assertEquals(
      PreviewWatchDecision.decide(
        PreviewWatchScope.Parent(parent),
        StandardWatchEventKinds.ENTRY_MODIFY,
        Some(Path.of("sibling")),
        source,
      ),
      PreviewWatchDecision(
        invalidate = false,
        registerCreatedDirectory = false,
      ),
    )
    assertEquals(
      PreviewWatchDecision.decide(
        PreviewWatchScope.Parent(parent),
        StandardWatchEventKinds.ENTRY_CREATE,
        Some(source),
        source,
      ),
      PreviewWatchDecision(
        invalidate = true,
        registerCreatedDirectory = true,
      ),
    )
    assert(
      PreviewWatchDecision
        .decide(
          PreviewWatchScope.Parent(parent),
          StandardWatchEventKinds.OVERFLOW,
          None,
          source,
        )
        .invalidate,
    )

  test("source create, modify, delete, and overflow are full invalidations"):
    val source = Path.of("docs")
    Vector(
      StandardWatchEventKinds.ENTRY_CREATE,
      StandardWatchEventKinds.ENTRY_MODIFY,
      StandardWatchEventKinds.ENTRY_DELETE,
      StandardWatchEventKinds.OVERFLOW,
    ).foreach: kind =>
      assert(
        PreviewWatchDecision
          .decide(
            PreviewWatchScope.Source(source),
            kind,
            Some(Path.of("guide")),
            source,
          )
          .invalidate,
      )

  test("recursively watches new directories and a recreated source root"):
    val parent  = Files.createTempDirectory("doctrellis-watch-")
    val source  = Files.createDirectory(parent.resolve("docs"))
    val nested  = Files.createDirectories(source.resolve("existing/nested"))
    val mailbox = BlockingPreviewEventMailbox()
    val watcher = PreviewSourceWatcher.open(source.toRealPath(), mailbox)
    try
      Files.writeString(nested.resolve("first.md"), "first")
      awaitInvalidationAndDrain(mailbox)

      val created = Files.createDirectory(source.resolve("created"))
      awaitInvalidationAndDrain(mailbox)
      Files.writeString(created.resolve("second.md"), "second")
      awaitInvalidationAndDrain(mailbox)

      GeneratedDirectory.delete(source)
      awaitInvalidationAndDrain(mailbox)
      Files.createDirectory(source)
      awaitInvalidationAndDrain(mailbox)
      Files.writeString(source.resolve("recreated.md"), "recreated")
      awaitInvalidationAndDrain(mailbox)
    finally
      watcher.close()
      GeneratedDirectory.delete(parent)

  test("does not follow a directory symlink outside the source root"):
    if !isWindows then
      val parent   = Files.createTempDirectory("doctrellis-watch-link-")
      val source   = Files.createDirectory(parent.resolve("docs"))
      val external = Files.createDirectory(parent.resolve("external"))
      val mailbox  = BlockingPreviewEventMailbox()
      val watcher  = PreviewSourceWatcher.open(source.toRealPath(), mailbox)
      try
        Files.createSymbolicLink(source.resolve("linked"), external)
        awaitInvalidationAndDrain(mailbox)
        Files.writeString(external.resolve("outside.md"), "outside")
        assertEquals(
          mailbox.pollBefore(System.nanoTime() + TimeUnit.SECONDS.toNanos(3L)),
          None,
        )
      finally
        watcher.close()
        GeneratedDirectory.delete(parent)

  test("generated output and publication staging siblings do not invalidate"):
    val parent  = Files.createTempDirectory("doctrellis-watch-output-")
    val source  = Files.createDirectory(parent.resolve("docs"))
    val mailbox = BlockingPreviewEventMailbox()
    val watcher = PreviewSourceWatcher.open(source.toRealPath(), mailbox)
    try
      Vector(
        parent.resolve("mdoc"),
        parent.resolve("site"),
        parent.resolve(".mdoc.doctrellis-session.staging"),
        parent.resolve(".site.doctrellis-session.staging"),
      ).foreach: directory =>
        Files.createDirectory(directory)
        Files.writeString(directory.resolve("generated"), "generated")

      assertEquals(
        mailbox.pollBefore(
          System.nanoTime() + TimeUnit.SECONDS.toNanos(3L),
        ),
        None,
      )

      Files.writeString(source.resolve("changed.md"), "changed")
      awaitInvalidation(mailbox)
    finally
      watcher.close()
      GeneratedDirectory.delete(parent)

  private def awaitInvalidationAndDrain(
      mailbox: PreviewEventMailbox,
  ): Unit =
    awaitInvalidation(mailbox)
    var draining = true
    while draining do
      mailbox.pollBefore(
        System.nanoTime() + TimeUnit.MILLISECONDS.toNanos(250L),
      ) match
        case Some(PreviewLoopEvent.Invalidated(_)) => ()
        case Some(other)                           =>
          fail(s"unexpected watcher event while draining: $other")
        case None => draining = false

  private def awaitInvalidation(mailbox: PreviewEventMailbox): Unit =
    val executor = Executors.newSingleThreadExecutor()
    try
      val future = executor.submit(
        new Callable[PreviewLoopEvent]:
          override def call(): PreviewLoopEvent = mailbox.take(),
      )
      future.get(8L, TimeUnit.SECONDS) match
        case PreviewLoopEvent.Invalidated(_) => ()
        case other => fail(s"unexpected watcher event: $other")
    finally executor.shutdownNow(): Unit

  private def isWindows: Boolean =
    System
      .getProperty("os.name", "")
      .toLowerCase(java.util.Locale.ROOT)
      .contains("win")
