package org.doctrellis.sbtplugin

import java.io.{ByteArrayOutputStream, PrintStream}
import java.net.{URL, URLClassLoader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}
import java.util.ServiceLoader

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import mdoc.interfaces.{DiagnosticSeverity, Mdoc}

private[sbtplugin] final class MdocDocumentFailure(message: String)
    extends IllegalStateException(message)

private[sbtplugin] final class MdocEngineFailure(cause: Throwable)
    extends IllegalStateException(
      s"DocTrellis mdoc engine failed: ${Option(cause.getMessage).filter(_.nonEmpty).getOrElse(cause.toString)}",
      cause,
    )

private[sbtplugin] final class MdocProcessor private (
    loader: IsolatedMdocClassLoader,
    engine: Mdoc,
    console: MdocConsole,
    variables: Map[String, String],
    info: String => Unit,
    warn: String => Unit,
) extends AutoCloseable:
  def process(
      sourceDirectory: Path,
      outputDirectory: Path,
  ): Unit =
    val markdownCount = MdocProcessor.processSourceTree(
      sourceDirectory,
      outputDirectory,
    ): (relative, input, target) =>
      MdocProcessor.evaluate(
        engine,
        console,
        relative,
        input,
        target,
        variables,
        info,
        warn,
      )
    info(s"DocTrellis: mdoc processed $markdownCount Markdown file(s)")

  override def close(): Unit =
    var failure = Option.empty[Throwable]

    def release(action: => Unit): Unit =
      try action
      catch
        case error: Throwable =>
          failure match
            case None           => failure = Some(error)
            case Some(existing) => existing.addSuppressed(error)

    release(engine.shutdown())
    release(console.close())
    release(loader.close())
    failure.foreach(throw _)

private[sbtplugin] object MdocProcessor:
  def open(
      workspaceDirectory: Path,
      classpath: Seq[Path],
      scalacOptions: Seq[String],
      settings: Seq[String],
      variables: Map[String, String],
      info: String => Unit,
      warn: String => Unit,
  ): MdocProcessor =
    val console = MdocConsole()
    val loader  = IsolatedMdocClassLoader(
      classpath.map(_.toUri.toURL).toArray,
      classOf[Mdoc].getClassLoader,
    )
    try
      val configured = configure(
        engine = load(loader),
        workspaceDirectory = workspaceDirectory,
        classpath = classpath,
        scalacOptions = scalacOptions,
        settings = settings,
        console = console,
      )
      new MdocProcessor(loader, configured, console, variables, info, warn)
    catch
      case error: Throwable =>
        def cleanup(action: => Unit): Unit =
          try action
          catch
            case cleanupError: Throwable => error.addSuppressed(cleanupError)
        cleanup(console.close())
        cleanup(loader.close())
        throw error

  private[sbtplugin] def processSourceTree(
      source: Path,
      output: Path,
  )(
      processMarkdown: (Path, Path, Path) => Unit,
  ): Int =
    val paths = list(source)
    paths.foreach: input =>
      val relative = source.relativize(input)
      val target   = output.resolve(relative)
      if Files.isDirectory(input, LinkOption.NOFOLLOW_LINKS) then
        Files.createDirectories(target): Unit
      else if Files.isRegularFile(input) then
        Files.createDirectories(target.getParent): Unit
        if isMarkdown(input) then processMarkdown(relative, input, target)
        else
          Files.copy(
            input,
            target,
            StandardCopyOption.REPLACE_EXISTING,
          ): Unit
    paths.count(isMarkdown)

  private def configure(
      engine: Mdoc,
      workspaceDirectory: Path,
      classpath: Seq[Path],
      scalacOptions: Seq[String],
      settings: Seq[String],
      console: MdocConsole,
  ): Mdoc =
    var current = engine
    try
      current = current.withWorkingDirectory(workspaceDirectory)
      current = current.withClasspath(classpath.asJava)
      current = current.withScalacOptions(scalacOptions.asJava)
      current = current.withSettings(settings.asJava)
      current = current.withConsoleReporter(console.stream)
      current
    catch
      case error: Throwable =>
        try current.shutdown()
        catch case cleanupError: Throwable => error.addSuppressed(cleanupError)
        throw error

  private def load(loader: ClassLoader): Mdoc =
    val providers = ServiceLoader.load(classOf[Mdoc], loader).iterator()
    if providers.hasNext then providers.next()
    else
      throw IllegalStateException(
        "DocTrellis could not load the mdoc programmatic implementation",
      )

  private def list(root: Path): Vector[Path] =
    val stream = Files.walk(root)
    try stream.iterator().asScala.toVector.sortBy(_.toString)
    finally stream.close()

  private def isMarkdown(path: Path): Boolean =
    val name = path.getFileName.toString.toLowerCase(java.util.Locale.ROOT)
    Files.isRegularFile(path) && (name.endsWith(".md") || name.endsWith(
      ".markdown",
    ))

  private def evaluate(
      engine: Mdoc,
      console: MdocConsole,
      relative: Path,
      input: Path,
      output: Path,
      variables: Map[String, String],
      info: String => Unit,
      warn: String => Unit,
  ): Unit =
    console.reset()
    val source                 = Files.readString(input, StandardCharsets.UTF_8)
    val (content, diagnostics) = MdocEngineEvaluation:
      val result = engine.evaluateMarkdownDocument(
        relative.toString,
        source,
        variables.asJava,
      )
      result.content() -> result.diagnostics().asScala.toVector
    val consoleOutput = MdocConsoleOutput.normalize(console.result())
    diagnostics.foreach: diagnostic =>
      val position = diagnostic.position()
      val message  =
        s"$relative:${position.startLine() + 1}:${position.startColumn() + 1}: ${diagnostic.message()}"
      if diagnostic.severity() == DiagnosticSeverity.Warning then warn(message)
      else if diagnostic.severity() == DiagnosticSeverity.Info then
        info(message)

    val diagnosticErrors = diagnostics
      .filter(_.severity() == DiagnosticSeverity.Error)
      .map: diagnostic =>
        val position = diagnostic.position()
        s"$relative:${position.startLine() + 1}:${position.startColumn() + 1}: ${diagnostic.message()}"
    val consoleErrors = Option
      .when(MdocConsoleOutput.hasErrors(consoleOutput))(
        s"$relative:\n$consoleOutput",
      )
      .toVector
    val errors = diagnosticErrors ++ consoleErrors
    if errors.nonEmpty then
      val rendered = errors
        .mkString("\n")
      throw MdocDocumentFailure(
        s"DocTrellis mdoc processing failed:\n$rendered",
      )

    if consoleOutput.nonEmpty then warn(s"$relative:\n$consoleOutput")

    Files.writeString(output, content, StandardCharsets.UTF_8): Unit

private[sbtplugin] object MdocEngineEvaluation:
  def apply[A](operation: => A): A =
    try operation
    catch case NonFatal(error) => throw MdocEngineFailure(error)

private[sbtplugin] final class MdocConsole private (
    buffer: ByteArrayOutputStream,
    val stream: PrintStream,
) extends AutoCloseable:
  def reset(): Unit = buffer.reset()

  def result(): String =
    stream.flush()
    buffer.toString(StandardCharsets.UTF_8)

  override def close(): Unit = stream.close()

private[sbtplugin] object MdocConsole:
  def apply(): MdocConsole =
    val buffer = ByteArrayOutputStream()
    new MdocConsole(
      buffer,
      PrintStream(buffer, true, StandardCharsets.UTF_8),
    )

private[sbtplugin] object MdocConsoleOutput:
  private val AnsiEscape = "\u001b\\[[0-9;]*m".r

  def normalize(value: String): String =
    AnsiEscape.replaceAllIn(value, "").trim

  def hasErrors(value: String): Boolean =
    value.linesIterator.exists(_.startsWith("error:"))

private[sbtplugin] object MdocResource:
  def use[A, B](resource: => A)(release: A => Unit)(operation: A => B): B =
    val acquired = resource
    var primary  = Option.empty[Throwable]
    try operation(acquired)
    catch
      case error: Throwable =>
        primary = Some(error)
        throw error
    finally
      try release(acquired)
      catch
        case cleanupError: Throwable =>
          primary match
            case Some(error) => error.addSuppressed(cleanupError)
            case None        => throw cleanupError

private final class IsolatedMdocClassLoader(
    urls: Array[URL],
    parent: ClassLoader,
) extends URLClassLoader(urls, parent):
  override protected def loadClass(name: String, resolve: Boolean): Class[?] =
    getClassLoadingLock(name).synchronized:
      val loaded = Option(findLoadedClass(name)).getOrElse:
        if sharedWithParent(name) then super.loadClass(name, false)
        else
          try findClass(name)
          catch case NonFatal(_) => super.loadClass(name, false)
      if resolve then resolveClass(loaded)
      loaded

  private def sharedWithParent(name: String): Boolean =
    name.startsWith("mdoc.interfaces.") || name.startsWith("coursierapi.")
