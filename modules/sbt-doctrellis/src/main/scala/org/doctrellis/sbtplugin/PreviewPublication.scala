package org.doctrellis.sbtplugin

import java.io.IOException
import java.nio.ByteBuffer
import java.nio.channels.{FileChannel, FileLock, OverlappingFileLockException}
import java.nio.charset.StandardCharsets
import java.nio.file.{
  AtomicMoveNotSupportedException,
  Files,
  LinkOption,
  Path,
  StandardCopyOption,
  StandardOpenOption,
}
import java.util.UUID
import java.util.concurrent.atomic.{AtomicBoolean, AtomicLong}
import java.util.concurrent.locks.ReentrantReadWriteLock

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import sbt.Logger

private[sbtplugin] final class PreviewPublicationLock:
  private val underlying = ReentrantReadWriteLock(true)

  def read[A](operation: => A): A =
    val lock = underlying.readLock()
    lock.lock()
    try operation
    finally lock.unlock()

  def write[A](operation: => A): A =
    val lock = underlying.writeLock()
    lock.lock()
    try operation
    finally lock.unlock()

private[sbtplugin] final class PreviewPublicationCompromised(
    message: String,
    cause: Throwable,
) extends IllegalStateException(message, cause)

private[sbtplugin] final class PreviewPublication private (
    val lock: PreviewPublicationLock,
    mdocTarget: PreviewPublication.OwnedTarget,
    siteTarget: PreviewPublication.OwnedTarget,
    transaction: PreviewPublication.PublicationTransaction,
    lease: PreviewPublication.PublicationLease,
    moveDirectory: (Path, Path) => Unit,
    log: Logger,
):
  private val closed      = AtomicBoolean(false)
  private val compromised = AtomicBoolean(false)
  private val revision    = AtomicLong(0L)
  private val targets     = Vector(mdocTarget, siteTarget)

  def stagedTarget: DocTrellisBuildTarget =
    DocTrellisBuildTarget(
      mdocDirectory = mdocTarget.staging,
      siteDirectory = siteTarget.staging,
    )

  def buildAndPublish(build: DocTrellisBuildTarget => Unit): Long =
    requireOpen()
    targets.foreach(_.prepareAttempt())
    transaction.prepareAttempt()
    try
      build(stagedTarget)
      val index = siteTarget.staging.resolve("index.html")
      if !Files.isRegularFile(index, LinkOption.NOFOLLOW_LINKS) then
        throw IllegalStateException(
          s"DocTrellis staged preview did not contain an index page at $index.",
        )
    catch
      case NonFatal(error) =>
        targets.foreach(_.cleanupStaging(error))
        throw error

    try lock.write(publishStaging())
    catch
      case NonFatal(error) =>
        targets.foreach(_.cleanupStaging(error))
        throw error
    val next = revision.incrementAndGet()
    targets.foreach(_.cleanupBackupAfterSuccess(log))
    log.success(
      s"DocTrellis published preview revision $next at ${siteTarget.destination}",
    )
    next

  def close(): Unit =
    if closed.compareAndSet(false, true) then
      var failure = Option.empty[Throwable]

      def cleanup(operation: => Unit): Unit =
        try operation
        catch
          case NonFatal(error) =>
            failure match
              case None           => failure = Some(error)
              case Some(existing) => existing.addSuppressed(error)

      targets.reverse.foreach: target =>
        if compromised.get() then cleanup(target.preserveBackupForRecovery())
        else cleanup(target.cleanupOwnedPaths())
      if failure.isEmpty && !compromised.get() then
        targets.reverse.foreach(target => cleanup(target.cleanupMarker()))
        if failure.isEmpty then cleanup(transaction.cleanupOwned())
      cleanup(lease.close())
      failure.foreach(throw _)

  private def publishStaging(): Unit =
    val backedUp  = ArrayBuffer.empty[PreviewPublication.OwnedTarget]
    val installed = ArrayBuffer.empty[PreviewPublication.OwnedTarget]
    try
      targets.foreach: target =>
        if Files.exists(target.destination, LinkOption.NOFOLLOW_LINKS) then
          moveDirectory(target.destination, target.backup)
          backedUp += target
      targets.foreach: target =>
        moveDirectory(target.staging, target.destination)
        installed += target
      transaction.markCommitted()
    catch
      case NonFatal(publicationError) =>
        var rollbackFailure = Option.empty[Throwable]

        def rollback(operation: => Unit): Unit =
          try operation
          catch
            case NonFatal(error) =>
              rollbackFailure match
                case None           => rollbackFailure = Some(error)
                case Some(existing) => existing.addSuppressed(error)

        installed.reverseIterator.foreach(target =>
          rollback(GeneratedDirectory.delete(target.destination)),
        )
        backedUp.reverseIterator.foreach(target =>
          rollback(moveDirectory(target.backup, target.destination)),
        )
        rollback(transaction.prepareAttempt())
        rollbackFailure match
          case Some(error) =>
            compromised.set(true): Unit
            publicationError.addSuppressed(error)
            throw PreviewPublicationCompromised(
              "DocTrellis could not restore the last successful preview after publication failed.",
              publicationError,
            )
          case None => throw publicationError

  private def requireOpen(): Unit =
    if closed.get() then
      throw IllegalStateException("DocTrellis preview publication is closed.")

private[sbtplugin] object PreviewPublication:
  private val MarkerVersion = "doctrellis-preview-publication-v1"

  private final case class LeaseHandle(
      channel: FileChannel,
      lock: FileLock,
  )

  private final class PublicationLease private (
      handles: Vector[LeaseHandle],
  ):
    def close(): Unit =
      var failure = Option.empty[Throwable]

      def cleanup(operation: => Unit): Unit =
        try operation
        catch
          case NonFatal(error) =>
            failure match
              case None           => failure = Some(error)
              case Some(existing) => existing.addSuppressed(error)

      handles.reverse.foreach: handle =>
        cleanup(handle.lock.release())
        cleanup(handle.channel.close())
      failure.foreach(throw _)

  private object PublicationLease:
    def use[A](destinations: Vector[Path])(operation: => A): A =
      val lease   = acquire(destinations)
      var primary = Option.empty[Throwable]
      try operation
      catch
        case error: Throwable =>
          primary = Some(error)
          throw error
      finally
        try lease.close()
        catch
          case cleanupError: Throwable =>
            primary match
              case Some(error) => error.addSuppressed(cleanupError)
              case None        => throw cleanupError

    def acquire(destinations: Vector[Path]): PublicationLease =
      val paths = destinations
        .map(lockPath)
        .distinct
        .sortBy(_.toString)
      val acquired = ArrayBuffer.empty[LeaseHandle]
      try
        paths.foreach: path =>
          Files.createDirectories(path.getParent)
          if Files.isSymbolicLink(path) then
            throw IllegalStateException(
              s"DocTrellis publication lock must not be a symbolic link: $path",
            )
          val channel = FileChannel.open(
            path,
            StandardOpenOption.CREATE,
            StandardOpenOption.WRITE,
          )
          try
            val lock = try channel.tryLock()
            catch
              case _: OverlappingFileLockException =>
                throw IllegalStateException(
                  s"Another DocTrellis build or preview is already publishing '$path'.",
                )
            if lock == null then
              throw IllegalStateException(
                s"Another DocTrellis build or preview is already publishing '$path'.",
              )
            acquired += LeaseHandle(channel, lock)
          catch
            case error: Throwable =>
              try channel.close()
              catch
                case NonFatal(cleanupError) =>
                  error.addSuppressed(cleanupError)
              throw error
        PublicationLease(acquired.toVector)
      catch
        case error: Throwable =>
          try PublicationLease(acquired.toVector).close()
          catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)
          throw error

  private final class OwnedTarget(
      val destination: Path,
      val staging: Path,
      val backup: Path,
      marker: Path,
  ):
    private val markerTemporary =
      marker.resolveSibling(s".${marker.getFileName}.tmp")

    def prepareAttempt(): Unit =
      GeneratedDirectory.delete(staging)
      GeneratedDirectory.delete(backup)

    def cleanupStaging(primary: Throwable): Unit =
      try GeneratedDirectory.delete(staging)
      catch case NonFatal(error) => primary.addSuppressed(error)

    def cleanupBackupAfterSuccess(log: Logger): Unit =
      try GeneratedDirectory.delete(backup)
      catch
        case NonFatal(error) =>
          log.warn(
            s"DocTrellis could not clean preview backup '$backup': ${error.getMessage}",
          )

    def preserveBackupForRecovery(): Unit =
      GeneratedDirectory.delete(staging)
      GeneratedDirectory.delete(markerTemporary)

    def cleanupOwnedPaths(): Unit =
      var failure = Option.empty[Throwable]
      Vector(staging, backup, markerTemporary).foreach: path =>
        try GeneratedDirectory.delete(path)
        catch
          case NonFatal(error) =>
            failure match
              case None           => failure = Some(error)
              case Some(existing) => existing.addSuppressed(error)
      failure.foreach(throw _)

    def cleanupMarker(): Unit = GeneratedDirectory.delete(marker)

    def cleanupOwned(): Unit =
      cleanupOwnedPaths()
      cleanupMarker()

  private final class PublicationTransaction private (
      marker: Path,
      markerTemporary: Path,
      contents: String,
  ):
    def prepareAttempt(): Unit =
      GeneratedDirectory.delete(markerTemporary)
      GeneratedDirectory.delete(marker)

    def markCommitted(): Unit =
      GeneratedDirectory.delete(markerTemporary)
      try
        Files.writeString(
          markerTemporary,
          contents,
          StandardCharsets.UTF_8,
          StandardOpenOption.CREATE_NEW,
          StandardOpenOption.WRITE,
        )
        replace(markerTemporary, marker)
      finally Files.deleteIfExists(markerTemporary): Unit

    def isCommitted: Boolean =
      markerMatches(marker, contents)

    def cleanupOwned(): Unit =
      GeneratedDirectory.delete(markerTemporary)
      GeneratedDirectory.delete(marker)

  private object PublicationTransaction:
    def apply(
        destinations: Vector[Path],
        sessionId: String,
    ): PublicationTransaction =
      val normalized = destinations.map(_.toAbsolutePath.normalize())
      val site       = normalized.lastOption.getOrElse(
        throw IllegalArgumentException(
          "DocTrellis preview publication requires at least one target.",
        ),
      )
      val marker = site.getParent.resolve(
        s".${site.getFileName}.doctrellis-$sessionId.commit",
      )
      new PublicationTransaction(
        marker = marker,
        markerTemporary = marker.resolveSibling(s".${marker.getFileName}.tmp"),
        contents = transactionContents(normalized, sessionId),
      )

  def open(
      layout: DocTrellisLayout,
      log: Logger,
      moveDirectory: (Path, Path) => Unit = move,
      replaceMarker: (Path, Path) => Unit = replace,
  ): PreviewPublication =
    val sessionId    = UUID.randomUUID().toString
    val destinations = Vector(layout.mdocDirectory, layout.siteDirectory)
    val lease        = PublicationLease.acquire(destinations)
    val transaction  = PublicationTransaction(destinations, sessionId)
    var acquiredMdoc = Option.empty[OwnedTarget]
    var acquiredSite = Option.empty[OwnedTarget]
    try
      cleanupStale(destinations)
      val mdocTarget = createTarget(
        layout.mdocDirectory,
        sessionId,
        replaceMarker,
      )
      acquiredMdoc = Some(mdocTarget)
      val siteTarget = createTarget(
        layout.siteDirectory,
        sessionId,
        replaceMarker,
      )
      acquiredSite = Some(siteTarget)
      PreviewPublication(
        lock = PreviewPublicationLock(),
        mdocTarget = mdocTarget,
        siteTarget = siteTarget,
        transaction = transaction,
        lease = lease,
        moveDirectory = moveDirectory,
        log = log,
      )
    catch
      case error: Throwable =>
        Vector(acquiredSite, acquiredMdoc).flatten.foreach: target =>
          try target.cleanupOwned()
          catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)
        try transaction.cleanupOwned()
        catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)
        try lease.close()
        catch case NonFatal(cleanupError) => error.addSuppressed(cleanupError)
        throw error

  def withOutputLease[A](
      target: DocTrellisBuildTarget,
  )(operation: => A): A =
    val destinations = Vector(target.mdocDirectory, target.siteDirectory)
    PublicationLease.use(destinations):
      cleanupStale(destinations)
      operation

  private def createTarget(
      destination: Path,
      sessionId: String,
      replaceMarker: (Path, Path) => Unit,
  ): OwnedTarget =
    val normalized = destination.toAbsolutePath.normalize()
    val parent     = Option(normalized.getParent).getOrElse(
      throw IllegalArgumentException(
        s"DocTrellis generated directory must have a parent: $normalized",
      ),
    )
    Files.createDirectories(parent)
    val prefix  = ownedPrefix(normalized, sessionId)
    val staging = parent.resolve(s"$prefix.staging")
    val backup  = parent.resolve(s"$prefix.backup")
    val marker  = parent.resolve(s"$prefix.owner")
    writeMarkerAtomically(
      marker,
      markerContents(normalized, sessionId),
      replaceMarker,
    )
    OwnedTarget(normalized, staging, backup, marker)

  private final case class StaleOwnedTarget(
      destination: Path,
      staging: Path,
      backup: Path,
      marker: Path,
      markerTemporary: Path,
      sessionId: String,
  ):
    def recover(committed: Boolean): Unit =
      if Files.isDirectory(backup, LinkOption.NOFOLLOW_LINKS) then
        if committed && Files.exists(destination, LinkOption.NOFOLLOW_LINKS)
        then GeneratedDirectory.delete(backup)
        else
          GeneratedDirectory.delete(destination)
          move(backup, destination)
      GeneratedDirectory.delete(staging)
      GeneratedDirectory.delete(backup)
      GeneratedDirectory.delete(markerTemporary)

    def cleanupMarker(): Unit = GeneratedDirectory.delete(marker)

  private def cleanupStale(destinations: Vector[Path]): Unit =
    val normalized = destinations.map(_.toAbsolutePath.normalize())
    val stale      = normalized.flatMap(staleTargets)
    stale
      .groupBy(_.sessionId)
      .foreach: (sessionId, targets) =>
        val transaction = PublicationTransaction(normalized, sessionId)
        val committed   = transaction.isCommitted
        targets.foreach(_.recover(committed))
        targets.foreach(_.cleanupMarker())
        transaction.cleanupOwned()
    cleanupOrphanTransactions(normalized)

  private def cleanupOrphanTransactions(destinations: Vector[Path]): Unit =
    destinations.lastOption.foreach: site =>
      val parent = site.getParent
      if Files.isDirectory(parent, LinkOption.NOFOLLOW_LINKS) then
        val markerPrefix = s".${site.getFileName}.doctrellis-"
        val stream       = Files.list(parent)
        try
          stream
            .iterator()
            .asScala
            .foreach: marker =>
              val name = marker.getFileName.toString
              if name.startsWith(markerPrefix) && name.endsWith(".commit") then
                val sessionId = name
                  .stripPrefix(markerPrefix)
                  .stripSuffix(".commit")
                if validSessionId(sessionId) then
                  val transaction = PublicationTransaction(
                    destinations,
                    sessionId,
                  )
                  if transaction.isCommitted then transaction.cleanupOwned()
        finally stream.close()

  private def staleTargets(destination: Path): Vector[StaleOwnedTarget] =
    val normalized = destination.toAbsolutePath.normalize()
    Option(normalized.getParent).toVector.flatMap: parent =>
      if Files.isDirectory(parent, LinkOption.NOFOLLOW_LINKS) then
        val markerPrefix = s".${normalized.getFileName}.doctrellis-"
        val stream       = Files.list(parent)
        try
          stream
            .iterator()
            .asScala
            .flatMap: marker =>
              val name = marker.getFileName.toString
              if name.startsWith(markerPrefix) && name.endsWith(".owner") then
                val sessionId = name
                  .stripPrefix(markerPrefix)
                  .stripSuffix(".owner")
                Option.when(
                  validSessionId(sessionId) && owns(
                    marker,
                    normalized,
                    sessionId,
                  ),
                ):
                  val prefix          = ownedPrefix(normalized, sessionId)
                  val staging         = parent.resolve(s"$prefix.staging")
                  val backup          = parent.resolve(s"$prefix.backup")
                  val markerTemporary = marker.resolveSibling(
                    s".${marker.getFileName}.tmp",
                  )
                  StaleOwnedTarget(
                    destination = normalized,
                    staging = staging,
                    backup = backup,
                    marker = marker,
                    markerTemporary = markerTemporary,
                    sessionId = sessionId,
                  )
              else None
            .toVector
        finally stream.close()
      else Vector.empty

  private def owns(
      marker: Path,
      destination: Path,
      sessionId: String,
  ): Boolean =
    markerMatches(
      marker,
      markerContents(
        destination,
        sessionId,
      ),
    )

  private def markerMatches(marker: Path, contents: String): Boolean =
    Files.isRegularFile(marker, LinkOption.NOFOLLOW_LINKS) &&
      (try Files.readString(marker, StandardCharsets.UTF_8) == contents
      catch case _: IOException => false)

  private def writeMarkerAtomically(
      marker: Path,
      contents: String,
      replaceMarker: (Path, Path) => Unit,
  ): Unit =
    val temporary = marker.resolveSibling(s".${marker.getFileName}.tmp")
    var owned     = false
    var primary   = Option.empty[Throwable]
    try
      val channel = FileChannel.open(
        temporary,
        StandardOpenOption.CREATE_NEW,
        StandardOpenOption.WRITE,
      )
      owned = true
      var channelPrimary = Option.empty[Throwable]
      try
        val bytes = ByteBuffer.wrap(contents.getBytes(StandardCharsets.UTF_8))
        while bytes.hasRemaining do
          val _ = channel.write(bytes)
        channel.force(true)
      catch
        case error: Throwable =>
          channelPrimary = Some(error)
          throw error
      finally
        try channel.close()
        catch
          case cleanupError: Throwable =>
            channelPrimary match
              case Some(error) => error.addSuppressed(cleanupError)
              case None        => throw cleanupError
      replaceMarker(temporary, marker)
      owned = false
    catch
      case error: Throwable =>
        primary = Some(error)
        throw error
    finally
      if owned then
        try Files.deleteIfExists(temporary): Unit
        catch
          case cleanupError: Throwable =>
            primary match
              case Some(error) => error.addSuppressed(cleanupError)
              case None        => throw cleanupError

  private def validSessionId(value: String): Boolean =
    try UUID.fromString(value).toString == value
    catch case _: IllegalArgumentException => false

  private def markerContents(
      destination: Path,
      sessionId: String,
  ): String =
    s"$MarkerVersion\nsession=$sessionId\ntarget=$destination\n"

  private def transactionContents(
      destinations: Vector[Path],
      sessionId: String,
  ): String =
    val targets = destinations.zipWithIndex
      .map((destination, index) => s"target.$index=$destination")
      .mkString("\n")
    s"$MarkerVersion\nsession=$sessionId\n$targets\nstate=committed\n"

  private def ownedPrefix(destination: Path, sessionId: String): String =
    s".${destination.getFileName}.doctrellis-$sessionId"

  private def lockPath(destination: Path): Path =
    val normalized = destination.toAbsolutePath.normalize()
    Option(normalized.getParent)
      .map(
        _.resolve(
          s".${normalized.getFileName}.doctrellis-publication.lock",
        ),
      )
      .getOrElse(
        throw IllegalArgumentException(
          s"DocTrellis generated directory must have a parent: $normalized",
        ),
      )

  private def move(source: Path, destination: Path): Unit =
    try Files.move(source, destination, StandardCopyOption.ATOMIC_MOVE): Unit
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(source, destination): Unit

  private def replace(source: Path, destination: Path): Unit =
    try
      Files.move(
        source,
        destination,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING,
      ): Unit
    catch
      case _: AtomicMoveNotSupportedException =>
        Files.move(
          source,
          destination,
          StandardCopyOption.REPLACE_EXISTING,
        ): Unit
