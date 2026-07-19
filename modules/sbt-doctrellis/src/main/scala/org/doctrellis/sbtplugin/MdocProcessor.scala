package org.doctrellis.sbtplugin

import java.net.{URL, URLClassLoader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, LinkOption, Path, StandardCopyOption}
import java.util.ServiceLoader

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import mdoc.interfaces.{DiagnosticSeverity, Mdoc}

private[sbtplugin] object MdocProcessor:
  def process(
      workspaceDirectory: Path,
      sourceDirectory: Path,
      outputDirectory: Path,
      classpath: Seq[Path],
      scalacOptions: Seq[String],
      settings: Seq[String],
      variables: Map[String, String],
      info: String => Unit,
      warn: String => Unit,
  ): Unit =
    val source = sourceDirectory
    val output = outputDirectory

    MdocResource.use(
      IsolatedMdocClassLoader(
        classpath.map(_.toUri.toURL).toArray,
        classOf[Mdoc].getClassLoader,
      ),
    )(_.close()): loader =>
      MdocResource.use(
        configure(
          engine = load(loader),
          workspaceDirectory = workspaceDirectory,
          classpath = classpath,
          scalacOptions = scalacOptions,
          settings = settings,
        ),
      )(_.shutdown()): configured =>
        val markdownCount = processSourceTree(source, output):
          (relative, input, target) =>
            evaluate(
              configured,
              relative,
              input,
              target,
              variables,
              info,
              warn,
            )
        info(
          s"DocTrellis: mdoc processed $markdownCount Markdown file(s)",
        )

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
  ): Mdoc =
    var current = engine
    try
      current = current.withWorkingDirectory(workspaceDirectory)
      current = current.withClasspath(classpath.asJava)
      current = current.withScalacOptions(scalacOptions.asJava)
      current = current.withSettings(settings.asJava)
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
      relative: Path,
      input: Path,
      output: Path,
      variables: Map[String, String],
      info: String => Unit,
      warn: String => Unit,
  ): Unit =
    val result = engine.evaluateMarkdownDocument(
      relative.toString,
      Files.readString(input, StandardCharsets.UTF_8),
      variables.asJava,
    )
    val diagnostics = result.diagnostics().asScala.toVector
    diagnostics.foreach: diagnostic =>
      val position = diagnostic.position()
      val message  =
        s"$relative:${position.startLine() + 1}:${position.startColumn() + 1}: ${diagnostic.message()}"
      if diagnostic.severity() == DiagnosticSeverity.Warning then warn(message)
      else if diagnostic.severity() == DiagnosticSeverity.Info then
        info(message)

    val errors = diagnostics.filter(_.severity() == DiagnosticSeverity.Error)
    if errors.nonEmpty then
      val rendered = errors
        .map: diagnostic =>
          val position = diagnostic.position()
          s"$relative:${position.startLine() + 1}:${position.startColumn() + 1}: ${diagnostic.message()}"
        .mkString("\n")
      throw IllegalStateException(
        s"DocTrellis mdoc processing failed:\n$rendered",
      )

    Files.writeString(output, result.content(), StandardCharsets.UTF_8): Unit

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
