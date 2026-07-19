package org.doctrellis.sbtplugin

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

import scala.util.control.NonFatal

import sbt.Logger

private[sbtplugin] enum PreviewLoopEvent:
  case Invalidated(atNanos: Long)
  case WatcherFailed(error: Throwable)
  case Stop

private[sbtplugin] trait PreviewEventMailbox:
  def take(): PreviewLoopEvent
  def pollNow(): Option[PreviewLoopEvent]
  def pollBefore(deadlineNanos: Long): Option[PreviewLoopEvent]

private[sbtplugin] final class BlockingPreviewEventMailbox(
    clockNanos: () => Long = () => System.nanoTime(),
) extends PreviewEventMailbox:
  private val events           = LinkedBlockingQueue[PreviewLoopEvent]()
  private val failure          = AtomicReference[Option[Throwable]](None)
  private val invalidationLock = Object()

  def invalidate(): Unit =
    // Keep timestamp capture and enqueue atomic with the after-build empty
    // poll. Otherwise a producer can capture a during-build timestamp, pause,
    // and enqueue only after the drain has declared the mailbox empty. The
    // idle path would then debounce a change that must trigger immediately.
    invalidationLock.synchronized:
      events.offer(PreviewLoopEvent.Invalidated(clockNanos())): Unit

  def watcherFailed(error: Throwable): Unit =
    if failure.compareAndSet(None, Some(error)) then
      events.offer(PreviewLoopEvent.WatcherFailed(error)): Unit

  def stop(): Unit = events.offer(PreviewLoopEvent.Stop): Unit

  def throwIfWatcherFailed(): Unit = failure.get().foreach(throw _)

  override def take(): PreviewLoopEvent = events.take()

  override def pollNow(): Option[PreviewLoopEvent] =
    invalidationLock.synchronized(Option(events.poll()))

  override def pollBefore(deadlineNanos: Long): Option[PreviewLoopEvent] =
    pollNow().orElse:
      val remaining = deadlineNanos - System.nanoTime()
      if remaining <= 0L then None
      else Option(events.poll(remaining, TimeUnit.NANOSECONDS))

private[sbtplugin] object PreviewRebuildLoop:
  private enum QuietResult:
    case Build, Stop

  private enum AfterBuild:
    case Idle, Immediate, Stop
    case Debounce(atNanos: Long)

  val QuietPeriodNanos: Long = TimeUnit.MILLISECONDS.toNanos(150L)

  def run(
      mailbox: PreviewEventMailbox,
      quietPeriodNanos: Long,
      rebuild: () => Long,
      onPublished: Long => Unit,
      log: Logger,
      clockNanos: () => Long = () => System.nanoTime(),
  ): Unit =
    var running           = true
    var buildImmediately  = false
    var pendingDebounceAt = Option.empty[Long]

    def schedule(result: AfterBuild): Unit = result match
      case AfterBuild.Idle              => ()
      case AfterBuild.Immediate         => buildImmediately = true
      case AfterBuild.Stop              => running = false
      case AfterBuild.Debounce(atNanos) =>
        pendingDebounceAt = Some(atNanos)

    def rebuildOnce(): AfterBuild =
      val completedAtNanos =
        try
          val revision  = rebuild()
          val completed = clockNanos()
          try onPublished(revision)
          catch
            case NonFatal(error) =>
              log.warn(
                s"DocTrellis published revision $revision but could not notify preview clients: ${error.getMessage}",
              )
          completed
        catch
          case compromised: PreviewPublicationCompromised =>
            throw compromised
          case NonFatal(error) =>
            log.error(
              s"DocTrellis rebuild failed; continuing to serve the last successful site: ${error.getMessage}",
            )
            log.trace(error)
            clockNanos()
      afterBuild(mailbox, completedAtNanos)

    while running do
      if buildImmediately then
        buildImmediately = false
        schedule(rebuildOnce())
      else
        val event = pendingDebounceAt
          .map(PreviewLoopEvent.Invalidated(_))
          .getOrElse(mailbox.take())
        pendingDebounceAt = None
        event match
          case PreviewLoopEvent.Invalidated(atNanos) =>
            awaitQuiet(mailbox, atNanos, quietPeriodNanos) match
              case QuietResult.Stop  => running = false
              case QuietResult.Build => schedule(rebuildOnce())
          case PreviewLoopEvent.WatcherFailed(error) => throw error
          case PreviewLoopEvent.Stop                 => running = false

  private def afterBuild(
      mailbox: PreviewEventMailbox,
      completedAtNanos: Long,
  ): AfterBuild =
    var immediate       = false
    var latestPostBuild = Option.empty[Long]
    var result          = Option.empty[AfterBuild]
    while result.isEmpty do
      mailbox.pollNow() match
        case None =>
          result = Some:
            if immediate then AfterBuild.Immediate
            else
              latestPostBuild
                .map(AfterBuild.Debounce(_))
                .getOrElse(AfterBuild.Idle)
        case Some(PreviewLoopEvent.Invalidated(atNanos)) =>
          if atNanos <= completedAtNanos then immediate = true
          else
            latestPostBuild = Some(
              latestPostBuild.fold(atNanos)(math.max(_, atNanos)),
            )
        case Some(PreviewLoopEvent.WatcherFailed(error)) => throw error
        case Some(PreviewLoopEvent.Stop)                 =>
          result = Some(AfterBuild.Stop)
    result.getOrElse(AfterBuild.Idle)

  private def awaitQuiet(
      mailbox: PreviewEventMailbox,
      firstInvalidation: Long,
      quietPeriodNanos: Long,
  ): QuietResult =
    var latest = firstInvalidation
    var result = Option.empty[QuietResult]
    while result.isEmpty do
      mailbox.pollBefore(saturatingAdd(latest, quietPeriodNanos)) match
        case None => result = Some(QuietResult.Build)
        case Some(PreviewLoopEvent.Invalidated(atNanos)) =>
          latest = math.max(latest, atNanos)
        case Some(PreviewLoopEvent.WatcherFailed(error)) => throw error
        case Some(PreviewLoopEvent.Stop)                 =>
          result = Some(QuietResult.Stop)
    result.getOrElse(QuietResult.Stop)

  private def saturatingAdd(left: Long, right: Long): Long =
    if right > 0L && left > Long.MaxValue - right then Long.MaxValue
    else left + right
