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
    withLockedPublicTarget(build)

  private[sbtplugin] def withLockedPublicTarget[A](
      operation: DocTrellisBuildTarget => A,
  ): A =
    val target = publicTarget
    PreviewPublication.withOutputLease(target):
      operation(target)

  def build(target: DocTrellisBuildTarget): Unit =
    GeneratedDirectory.reset(target.mdocDirectory)
    inputs.log.info(
      s"DocTrellis: running mdoc from ${inputs.layout.sourceDirectory}",
    )
    MdocProcessor.process(
      workspaceDirectory = inputs.layout.workspaceDirectory,
      sourceDirectory = inputs.layout.sourceDirectory,
      outputDirectory = target.mdocDirectory,
      classpath = inputs.classpath,
      scalacOptions = inputs.scalacOptions,
      settings = inputs.mdocArguments,
      variables = inputs.mdocVariables,
      info = inputs.log.info,
      warn = inputs.log.warn,
    )

    GeneratedDirectory.reset(target.siteDirectory)
    inputs.log.info(
      s"DocTrellis: rendering HTML with Laika and the Helium theme into ${target.siteDirectory}",
    )
    val rendered = try
      Transformer
        .from(Markdown)
        .to(HTML)
        .using(Markdown.GitHubFlavor, GfmTaskLists)
        .parallel[CatsIO]
        .withTheme(
          Helium.defaults.site.inlineCSS(GfmTaskLists.themeCSS).build,
        )
        .build
        .use(
          _.fromDirectory(target.mdocDirectory.toString)
            .toDirectory(target.siteDirectory.toString)
            .transform,
        )
        .unsafeRunSync()
    catch
      case NonFatal(error) =>
        throw IllegalStateException(
          s"DocTrellis Laika rendering failed: ${error.getMessage}",
          error,
        )

    inputs.log.info(
      s"DocTrellis: Laika rendered ${rendered.allDocuments.map(_.path).mkString(", ")}",
    )

    val index = target.siteDirectory.resolve("index.html")
    if !Files.isRegularFile(index, LinkOption.NOFOLLOW_LINKS) then
      throw IllegalStateException(
        s"DocTrellis did not generate an index page at $index",
      )
    inputs.log.success(s"DocTrellis site generated at ${target.siteDirectory}")

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
