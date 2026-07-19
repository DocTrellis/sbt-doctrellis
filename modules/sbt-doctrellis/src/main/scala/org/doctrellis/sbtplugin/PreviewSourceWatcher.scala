package org.doctrellis.sbtplugin

import java.nio.file.{
  ClosedWatchServiceException,
  FileVisitResult,
  Files,
  LinkOption,
  NoSuchFileException,
  NotDirectoryException,
  Path,
  SimpleFileVisitor,
  StandardWatchEventKinds,
  WatchEvent,
  WatchKey,
  WatchService,
}
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.{Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

private[sbtplugin] enum PreviewWatchScope:
  case Source(directory: Path)
  case Parent(directory: Path)

private[sbtplugin] final case class PreviewWatchDecision(
    invalidate: Boolean,
    registerCreatedDirectory: Boolean,
)

private[sbtplugin] object PreviewWatchDecision:
  def decide(
      scope: PreviewWatchScope,
      kind: WatchEvent.Kind[?],
      context: Option[Path],
      sourceName: Path,
  ): PreviewWatchDecision =
    if kind == StandardWatchEventKinds.OVERFLOW then
      PreviewWatchDecision(
        invalidate = true,
        registerCreatedDirectory = false,
      )
    else
      scope match
        case PreviewWatchScope.Parent(_) =>
          val relevant = context.contains(sourceName)
          PreviewWatchDecision(
            invalidate = relevant,
            registerCreatedDirectory = relevant &&
              kind == StandardWatchEventKinds.ENTRY_CREATE,
          )
        case PreviewWatchScope.Source(_) =>
          PreviewWatchDecision(
            invalidate = kind == StandardWatchEventKinds.ENTRY_CREATE ||
              kind == StandardWatchEventKinds.ENTRY_MODIFY ||
              kind == StandardWatchEventKinds.ENTRY_DELETE,
            registerCreatedDirectory =
              kind == StandardWatchEventKinds.ENTRY_CREATE,
          )

private[sbtplugin] final class PreviewSourceWatcher private (
    watchService: WatchService,
    executor: java.util.concurrent.ExecutorService,
    closed: AtomicBoolean,
):
  def close(): Unit =
    if closed.compareAndSet(false, true) then
      var failure = Option.empty[Throwable]
      try watchService.close()
      catch case NonFatal(error) => failure = Some(error)
      try executor.shutdownNow(): Unit
      catch
        case NonFatal(error) =>
          failure match
            case None           => failure = Some(error)
            case Some(existing) => existing.addSuppressed(error)
      try
        if !executor.awaitTermination(5L, TimeUnit.SECONDS) then
          throw IllegalStateException(
            "DocTrellis source watcher did not terminate during shutdown.",
          )
      catch
        case error: InterruptedException =>
          Thread.currentThread().interrupt()
          failure match
            case None           => failure = Some(error)
            case Some(existing) => existing.addSuppressed(error)
        case NonFatal(error) =>
          failure match
            case None           => failure = Some(error)
            case Some(existing) => existing.addSuppressed(error)
      failure.foreach(throw _)

private[sbtplugin] object PreviewSourceWatcher:
  def open(
      sourceRoot: Path,
      mailbox: BlockingPreviewEventMailbox,
  ): PreviewSourceWatcher =
    val root   = sourceRoot.toAbsolutePath.normalize()
    val parent = Option(root.getParent).getOrElse(
      throw IllegalArgumentException(
        s"DocTrellis source directory must have a parent: $root",
      ),
    )
    val service  = root.getFileSystem.newWatchService()
    val closed   = AtomicBoolean(false)
    val executor = Executors.newSingleThreadExecutor: runnable =>
      val thread = Thread(runnable, "doctrellis-preview-watcher")
      thread.setDaemon(true)
      thread
    val runner = WatchRunner(
      sourceRoot = root,
      sourceParent = parent,
      watchService = service,
      mailbox = mailbox,
      closed = closed,
    )
    try
      runner.registerParent()
      runner.registerRecursively(root)
      executor.execute(() => runner.run())
      PreviewSourceWatcher(service, executor, closed)
    catch
      case error: Throwable =>
        closed.set(true)
        try service.close()
        catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)
        try executor.shutdownNow(): Unit
        catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)
        throw error

  private final class WatchRunner(
      sourceRoot: Path,
      sourceParent: Path,
      watchService: WatchService,
      mailbox: BlockingPreviewEventMailbox,
      closed: AtomicBoolean,
  ):
    private val registrations = mutable.Map.empty[WatchKey, PreviewWatchScope]

    def registerParent(): Unit =
      register(sourceParent, PreviewWatchScope.Parent(sourceParent))

    def registerRecursively(root: Path): Unit =
      if Files.isDirectory(root, LinkOption.NOFOLLOW_LINKS) then
        Files.walkFileTree(
          root,
          new SimpleFileVisitor[Path]:
            override def preVisitDirectory(
                directory: Path,
                attributes: BasicFileAttributes,
            ): FileVisitResult =
              if directory != root && Files.isSymbolicLink(directory) then
                FileVisitResult.SKIP_SUBTREE
              else
                register(directory, PreviewWatchScope.Source(directory))
                FileVisitResult.CONTINUE,
        ): Unit

    def run(): Unit =
      try
        while !closed.get() do
          val key = watchService.take()
          registrations.get(key) match
            case None        => key.reset(): Unit
            case Some(scope) =>
              process(key, scope)
              if !key.reset() then
                registrations.remove(key): Unit
                scope match
                  case PreviewWatchScope.Parent(_) if !closed.get() =>
                    throw IllegalStateException(
                      s"DocTrellis can no longer watch the source parent directory '$sourceParent'.",
                    )
                  case _ => ()
      catch
        case _: ClosedWatchServiceException if closed.get() => ()
        case _: InterruptedException if closed.get()        =>
          Thread.currentThread().interrupt()
        case error: InterruptedException =>
          Thread.currentThread().interrupt()
          mailbox.watcherFailed(
            IllegalStateException(
              "DocTrellis source watcher was interrupted unexpectedly.",
              error,
            ),
          )
        case NonFatal(error) =>
          mailbox.watcherFailed(
            IllegalStateException(
              s"DocTrellis source watcher failed for '$sourceRoot': ${error.getMessage}",
              error,
            ),
          )

    private def process(key: WatchKey, scope: PreviewWatchScope): Unit =
      key
        .pollEvents()
        .asScala
        .foreach: event =>
          val context = event.context() match
            case path: Path => Some(path)
            case _          => None
          val decision = PreviewWatchDecision.decide(
            scope = scope,
            kind = event.kind(),
            context = context,
            sourceName = sourceRoot.getFileName,
          )
          if decision.registerCreatedDirectory then
            context.foreach(path => registerCreated(scope, path))
          if event.kind() == StandardWatchEventKinds.OVERFLOW then
            attemptTransient(registerRecursively(sourceRoot))
          if decision.invalidate then mailbox.invalidate()

    private def registerCreated(scope: PreviewWatchScope, context: Path): Unit =
      val created = scope match
        case PreviewWatchScope.Source(directory) => directory.resolve(context)
        case PreviewWatchScope.Parent(_)         => sourceRoot
      attemptTransient(registerRecursively(created))

    private def attemptTransient(operation: => Unit): Unit =
      try operation
      catch case _: NoSuchFileException | _: NotDirectoryException => ()

    private def register(directory: Path, scope: PreviewWatchScope): Unit =
      val key = directory.register(
        watchService,
        StandardWatchEventKinds.ENTRY_CREATE,
        StandardWatchEventKinds.ENTRY_MODIFY,
        StandardWatchEventKinds.ENTRY_DELETE,
      )
      registrations.update(key, scope)
