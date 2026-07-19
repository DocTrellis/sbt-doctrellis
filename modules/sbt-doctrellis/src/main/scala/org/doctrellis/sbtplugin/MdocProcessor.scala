package org.doctrellis.sbtplugin

import java.net.{URL, URLClassLoader}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.ServiceLoader

import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

import mdoc.interfaces.{DiagnosticSeverity, Mdoc}

private[sbtplugin] object MdocProcessor:
  def process(
      layout: DocTrellisLayout,
      classpath: Seq[Path],
      scalacOptions: Seq[String],
      settings: Seq[String],
      variables: Map[String, String],
      info: String => Unit,
      warn: String => Unit,
  ): Unit =
    val source = layout.sourceDirectory
    val output = layout.mdocDirectory

    val loader = IsolatedMdocClassLoader(
      classpath.map(_.toUri.toURL).toArray,
      classOf[Mdoc].getClassLoader,
    )
    val engine = load(loader)
      .withWorkingDirectory(layout.workspaceDirectory)
      .withClasspath(classpath.asJava)
      .withScalacOptions(scalacOptions.asJava)
      .withSettings(settings.asJava)

    try
      val paths = list(source)
      paths.foreach: input =>
        val target = output.resolve(source.relativize(input))
        if Files.isDirectory(input) then Files.createDirectories(target)
        else if Files.isRegularFile(input) then
          Files.createDirectories(target.getParent)
          if isMarkdown(input) then
            evaluate(
              engine,
              source.relativize(input),
              input,
              target,
              variables,
              info,
              warn,
            )
          else Files.copy(input, target, StandardCopyOption.REPLACE_EXISTING)
      info(
        s"DocTrellis: mdoc processed ${paths.count(isMarkdown)} Markdown file(s)",
      )
    finally
      engine.shutdown()
      loader.close()

  private def load(loader: ClassLoader): Mdoc =
    val providers = ServiceLoader.load(classOf[Mdoc], loader).iterator()
    if providers.hasNext then providers.next()
    else
      sys.error(
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
      sys.error(s"DocTrellis mdoc processing failed:\n$rendered")

    Files.writeString(output, result.content(), StandardCharsets.UTF_8)

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
