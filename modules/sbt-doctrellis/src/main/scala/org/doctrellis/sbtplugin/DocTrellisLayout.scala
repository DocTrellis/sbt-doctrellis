package org.doctrellis.sbtplugin

import java.nio.file.{Files, Path}
import scala.util.control.NonFatal

private[sbtplugin] enum DocTrellisDirectoryRole:
  case Workspace, Source, Mdoc, Site

  def description: String = this match
    case Workspace => "workspace directory"
    case Source    => "source directory"
    case Mdoc      => "mdoc directory"
    case Site      => "site output directory"

private[sbtplugin] enum DocTrellisLayoutProblem:
  case CannotCanonicalize(
      role: DocTrellisDirectoryRole,
      path: Path,
      detail: String,
  )
  case MissingDirectory(role: DocTrellisDirectoryRole, path: Path)
  case NotDirectory(role: DocTrellisDirectoryRole, path: Path)
  case GeneratedDirectoryContainsWorkspace(
      role: DocTrellisDirectoryRole,
      generated: Path,
      workspace: Path,
  )
  case OverlappingDirectories(
      firstRole: DocTrellisDirectoryRole,
      first: Path,
      secondRole: DocTrellisDirectoryRole,
      second: Path,
  )

  def message: String = this match
    case CannotCanonicalize(role, path, detail) =>
      s"DocTrellis could not canonicalize the ${role.description} '$path': $detail"
    case MissingDirectory(role, path) =>
      s"DocTrellis ${role.description} does not exist: $path"
    case NotDirectory(role, path) =>
      s"DocTrellis ${role.description} is not a directory: $path"
    case GeneratedDirectoryContainsWorkspace(role, generated, workspace) =>
      s"DocTrellis ${role.description} '$generated' must not equal or contain the workspace '$workspace'"
    case OverlappingDirectories(
          firstRole,
          first,
          secondRole,
          second,
        ) =>
      s"DocTrellis ${firstRole.description} '$first' must not overlap the ${secondRole.description} '$second'"

private[sbtplugin] final class DocTrellisLayout private (
    val workspaceDirectory: Path,
    val sourceDirectory: Path,
    val mdocDirectory: Path,
    val siteDirectory: Path,
)

private[sbtplugin] object DocTrellisLayout:
  private final class DirectoryPath(
      val lexical: Path,
      val canonical: Path,
      val canonicalAncestors: Vector[Path],
  ):
    val representations: Vector[Path] = Vector(lexical, canonical).distinct

  def parse(
      workspaceDirectory: Path,
      sourceDirectory: Path,
      mdocDirectory: Path,
      siteDirectory: Path,
  ): Either[DocTrellisLayoutProblem, DocTrellisLayout] =
    for
      workspace <- resolve(
        DocTrellisDirectoryRole.Workspace,
        workspaceDirectory,
      )
      source <- resolve(
        DocTrellisDirectoryRole.Source,
        sourceDirectory,
      )
      mdoc <- resolve(DocTrellisDirectoryRole.Mdoc, mdocDirectory)
      site <- resolve(DocTrellisDirectoryRole.Site, siteDirectory)
      _    <- requireExistingDirectory(
        DocTrellisDirectoryRole.Workspace,
        workspace,
      )
      _ <- requireExistingDirectory(DocTrellisDirectoryRole.Source, source)
      _ <- requireDirectoryIfPresent(DocTrellisDirectoryRole.Mdoc, mdoc)
      _ <- requireDirectoryIfPresent(DocTrellisDirectoryRole.Site, site)
      _ <- rejectWorkspaceContainer(
        DocTrellisDirectoryRole.Mdoc,
        mdoc,
        workspace,
      )
      _ <- rejectWorkspaceContainer(
        DocTrellisDirectoryRole.Site,
        site,
        workspace,
      )
      _ <- rejectOverlap(
        DocTrellisDirectoryRole.Mdoc,
        mdoc,
        DocTrellisDirectoryRole.Source,
        source,
      )
      _ <- rejectOverlap(
        DocTrellisDirectoryRole.Site,
        site,
        DocTrellisDirectoryRole.Source,
        source,
      )
      _ <- rejectOverlap(
        DocTrellisDirectoryRole.Mdoc,
        mdoc,
        DocTrellisDirectoryRole.Site,
        site,
      )
    yield new DocTrellisLayout(
      workspaceDirectory = workspace.canonical,
      sourceDirectory = source.canonical,
      mdocDirectory = mdoc.canonical,
      siteDirectory = site.canonical,
    )

  private def resolve(
      role: DocTrellisDirectoryRole,
      path: Path,
  ): Either[DocTrellisLayoutProblem, DirectoryPath] =
    try
      val lexical   = path.toAbsolutePath.normalize()
      val ancestors = Vector.newBuilder[Path]
      var current   = Option(lexical)
      while current.nonEmpty do
        current.foreach(value =>
          ancestors += value.toFile.getCanonicalFile.toPath,
        )
        current = current.flatMap(value => Option(value.getParent))
      Right(
        new DirectoryPath(
          lexical = lexical,
          canonical = path.toFile.getCanonicalFile.toPath,
          canonicalAncestors = ancestors.result().distinct,
        ),
      )
    catch
      case NonFatal(error) =>
        Left(
          DocTrellisLayoutProblem.CannotCanonicalize(
            role,
            path,
            error.toString,
          ),
        )

  private def requireExistingDirectory(
      role: DocTrellisDirectoryRole,
      path: DirectoryPath,
  ): Either[DocTrellisLayoutProblem, Unit] =
    if !Files.exists(path.canonical) then
      Left(DocTrellisLayoutProblem.MissingDirectory(role, path.canonical))
    else if !Files.isDirectory(path.canonical) then
      Left(DocTrellisLayoutProblem.NotDirectory(role, path.canonical))
    else Right(())

  private def requireDirectoryIfPresent(
      role: DocTrellisDirectoryRole,
      path: DirectoryPath,
  ): Either[DocTrellisLayoutProblem, Unit] =
    Either.cond(
      !Files.exists(path.canonical) || Files.isDirectory(path.canonical),
      (),
      DocTrellisLayoutProblem.NotDirectory(role, path.canonical),
    )

  private def rejectWorkspaceContainer(
      role: DocTrellisDirectoryRole,
      generated: DirectoryPath,
      workspace: DirectoryPath,
  ): Either[DocTrellisLayoutProblem, Unit] =
    Either.cond(
      !contains(generated, workspace),
      (),
      DocTrellisLayoutProblem.GeneratedDirectoryContainsWorkspace(
        role,
        generated.canonical,
        workspace.canonical,
      ),
    )

  private def rejectOverlap(
      firstRole: DocTrellisDirectoryRole,
      first: DirectoryPath,
      secondRole: DocTrellisDirectoryRole,
      second: DirectoryPath,
  ): Either[DocTrellisLayoutProblem, Unit] =
    Either.cond(
      !contains(first, second) && !contains(second, first),
      (),
      DocTrellisLayoutProblem.OverlappingDirectories(
        firstRole,
        first.canonical,
        secondRole,
        second.canonical,
      ),
    )

  private def contains(
      container: DirectoryPath,
      nested: DirectoryPath,
  ): Boolean =
    representationPairs(container, nested).exists((containerPath, nestedPath) =>
      nestedPath.startsWith(containerPath),
    ) || nested.canonicalAncestors.contains(container.canonical)

  private def representationPairs(
      first: DirectoryPath,
      second: DirectoryPath,
  ): Vector[(Path, Path)] =
    first.representations.flatMap(firstPath =>
      second.representations.map(secondPath => firstPath -> secondPath),
    )
