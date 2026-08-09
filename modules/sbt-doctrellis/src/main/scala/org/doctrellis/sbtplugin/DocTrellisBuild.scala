package org.doctrellis.sbtplugin

import java.nio.file.{Files, LinkOption, Path}

import scala.util.control.NonFatal

import cats.effect.IO as CatsIO
import cats.effect.unsafe.implicits.global

import laika.api.Transformer
import laika.format.{HTML, Markdown}
import laika.helium.Helium
import laika.io.syntax.*
import sbt.Logger

private[sbtplugin] final case class DocTrellisBuildInputs(
    layout: DocTrellisLayout,
    classpath: Vector[Path],
    scalacOptions: Vector[String],
    mdocArguments: Vector[String],
    mdocVariables: Map[String, String],
    log: Logger,
)

private[sbtplugin] final case class DocTrellisBuildTarget(
    mdocDirectory: Path,
    siteDirectory: Path,
)

private[sbtplugin] final class DocTrellisBuildProgram(
    inputs: DocTrellisBuildInputs,
):
  def publicTarget: DocTrellisBuildTarget =
    DocTrellisBuildTarget(
      mdocDirectory = inputs.layout.mdocDirectory,
      siteDirectory = inputs.layout.siteDirectory,
    )

  def layout: DocTrellisLayout = inputs.layout

  def buildPublic(): Unit =
    withLockedPublicTarget: target =>
      MdocResource.use(openSession())(_.close())(_.build(target))

  private[sbtplugin] def withLockedPublicTarget[A](
      operation: DocTrellisBuildTarget => A,
  ): A =
    val target = publicTarget
    PreviewPublication.withOutputLease(target):
      operation(target)

  def build(target: DocTrellisBuildTarget): Unit =
    MdocResource.use(openSession())(_.close())(_.build(target))

  def openSession(): DocTrellisBuildSession =
    DocTrellisBuildSession.open(inputs)

private[sbtplugin] final class DocTrellisBuildSession private (
    inputs: DocTrellisBuildInputs,
    mdoc: MdocProcessor,
    laika: DocTrellisLaikaRenderer,
) extends AutoCloseable:
  def build(target: DocTrellisBuildTarget): Unit =
    GeneratedDirectory.reset(target.mdocDirectory)
    inputs.log.info(
      s"DocTrellis: running mdoc from ${inputs.layout.sourceDirectory}",
    )
    mdoc.process(
      sourceDirectory = inputs.layout.sourceDirectory,
      outputDirectory = target.mdocDirectory,
    )

    GeneratedDirectory.reset(target.siteDirectory)
    inputs.log.info(
      s"DocTrellis: rendering HTML with Laika and the Helium theme into ${target.siteDirectory}",
    )
    val renderedPaths = try
      laika.render(target.mdocDirectory, target.siteDirectory)
    catch
      case NonFatal(error) =>
        throw IllegalStateException(
          s"DocTrellis Laika rendering failed: ${error.getMessage}",
          error,
        )

    inputs.log.info(
      s"DocTrellis: Laika rendered ${renderedPaths.mkString(", ")}",
    )

    val index = target.siteDirectory.resolve("index.html")
    if !Files.isRegularFile(index, LinkOption.NOFOLLOW_LINKS) then
      throw IllegalStateException(
        s"DocTrellis did not generate an index page at $index",
      )
    inputs.log.success(s"DocTrellis site generated at ${target.siteDirectory}")

  override def close(): Unit =
    var failure = Option.empty[Throwable]

    def release(action: => Unit): Unit =
      try action
      catch
        case error: Throwable =>
          failure match
            case None           => failure = Some(error)
            case Some(existing) => existing.addSuppressed(error)

    release(laika.close())
    release(mdoc.close())
    failure.foreach(throw _)

private[sbtplugin] object DocTrellisBuildSession:
  def open(inputs: DocTrellisBuildInputs): DocTrellisBuildSession =
    val mdoc = MdocProcessor.open(
      workspaceDirectory = inputs.layout.workspaceDirectory,
      classpath = inputs.classpath,
      scalacOptions = inputs.scalacOptions,
      settings = inputs.mdocArguments,
      variables = inputs.mdocVariables,
      info = inputs.log.info,
      warn = inputs.log.warn,
    )
    try new DocTrellisBuildSession(inputs, mdoc, DocTrellisLaikaRenderer.open())
    catch
      case error: Throwable =>
        try mdoc.close()
        catch case cleanupError: Throwable => error.addSuppressed(cleanupError)
        throw error

private[sbtplugin] final class DocTrellisLaikaRenderer private (
    transform: (Path, Path) => Vector[String],
    release: CatsIO[Unit],
) extends AutoCloseable:
  def render(source: Path, destination: Path): Vector[String] =
    transform(source, destination)

  override def close(): Unit = release.unsafeRunSync()

private[sbtplugin] object DocTrellisLaikaRenderer:
  def open(): DocTrellisLaikaRenderer =
    val (transformer, release) = Transformer
      .from(Markdown)
      .to(HTML)
      .using(Markdown.GitHubFlavor, GfmTaskLists)
      .parallel[CatsIO]
      .withTheme(
        Helium.defaults.site.inlineCSS(GfmTaskLists.themeCSS).build,
      )
      .build
      .allocated
      .unsafeRunSync()
    new DocTrellisLaikaRenderer(
      transform = (source, destination) =>
        transformer
          .fromDirectory(source.toString)
          .toDirectory(destination.toString)
          .transform
          .map(_.allDocuments.map(_.path.toString).toVector)
          .unsafeRunSync(),
      release = release,
    )

private[sbtplugin] object GeneratedDirectory:
  def reset(path: Path): Unit =
    delete(path)
    Files.createDirectories(path): Unit

  def delete(path: Path): Unit =
    if Files.exists(path, java.nio.file.LinkOption.NOFOLLOW_LINKS) then
      val visitor = new java.nio.file.SimpleFileVisitor[Path]:
        override def visitFile(
            file: Path,
            attributes: java.nio.file.attribute.BasicFileAttributes,
        ): java.nio.file.FileVisitResult =
          Files.delete(file)
          java.nio.file.FileVisitResult.CONTINUE

        override def postVisitDirectory(
            directory: Path,
            error: java.io.IOException | Null,
        ): java.nio.file.FileVisitResult =
          if error != null then throw error
          Files.delete(directory)
          java.nio.file.FileVisitResult.CONTINUE

      Files.walkFileTree(path, visitor): Unit
