package org.doctrellis.sbtplugin

import java.nio.file.{Files, Path}
import java.util.UUID

import scala.jdk.CollectionConverters.*
import scala.util.Using

import sbt.util.LogExchange

class PreviewPublicationSuite extends munit.FunSuite:
  private val invalidUtf8 = Array(0xc3.toByte, 0x28.toByte)

  private val crashBoundaries = Vector(
    "after the mdoc backup"  -> 1,
    "after the site backup"  -> 2,
    "after the mdoc install" -> 3,
    "after the site install" -> 4,
  )

  test(
    "publishes complete generations without stale files and increments revisions",
  ):
    withLayout: layout =>
      Files.createDirectories(layout.mdocDirectory)
      Files.createDirectories(layout.siteDirectory)
      Files.writeString(layout.mdocDirectory.resolve("stale.md"), "stale")
      Files.writeString(layout.siteDirectory.resolve("stale.html"), "stale")
      Files.writeString(layout.siteDirectory.resolve("index.html"), "old")
      val publication = open(layout)
      try
        val first = publication.buildAndPublish(target =>
          writeGeneration(target, "first"),
        )
        assertEquals(first, 1L)
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "first",
        )
        assert(!Files.exists(layout.siteDirectory.resolve("stale.html")))
        assert(!Files.exists(layout.mdocDirectory.resolve("stale.md")))
        val transactions = transactionPaths(layout)
        assertEquals(transactions.size, 1)
        transactions.foreach(path =>
          assert(Files.readString(path).endsWith("state=committed\n")),
        )

        val second = publication.buildAndPublish(target =>
          writeGeneration(target, "second"),
        )
        assertEquals(second, 2L)
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "second",
        )
      finally publication.close()
      assertEquals(ownedPaths(layout), Vector.empty)

  test(
    "an initial staged build failure publishes nothing and consumes no revision",
  ):
    withLayout: layout =>
      val publication = open(layout)
      try
        intercept[IllegalStateException](
          publication.buildAndPublish: target =>
            Files.createDirectories(target.mdocDirectory)
            Files.createDirectories(target.siteDirectory)
            Files.writeString(target.siteDirectory.resolve("index.html"), "bad")
            throw IllegalStateException("initial build failed"),
        ): Unit
        assert(!Files.exists(layout.mdocDirectory))
        assert(!Files.exists(layout.siteDirectory))

        val firstSuccess = publication.buildAndPublish(target =>
          writeGeneration(target, "first success"),
        )
        assertEquals(firstSuccess, 1L)
      finally publication.close()

  test("a failed staged build preserves the last successful generation"):
    withLayout: layout =>
      val publication = open(layout)
      try
        assertEquals(
          publication.buildAndPublish(target =>
            writeGeneration(target, "good"),
          ),
          1L,
        )
        val failure = intercept[IllegalStateException](
          publication.buildAndPublish: target =>
            Files.createDirectories(target.siteDirectory)
            Files.writeString(target.siteDirectory.resolve("index.html"), "bad")
            throw IllegalStateException("broken build"),
        )
        assertEquals(failure.getMessage, "broken build")
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "good",
        )
        assertEquals(
          publication.buildAndPublish(target =>
            writeGeneration(target, "recovered"),
          ),
          2L,
        )
      finally publication.close()

  test("a staged generation without index.html is never published"):
    withLayout: layout =>
      Files.createDirectories(layout.siteDirectory)
      Files.writeString(layout.siteDirectory.resolve("index.html"), "old")
      val publication = open(layout)
      try
        val failure = intercept[IllegalStateException](
          publication.buildAndPublish: target =>
            Files.createDirectories(target.mdocDirectory)
            Files.createDirectories(target.siteDirectory)
            Files.writeString(
              target.siteDirectory.resolve("other.html"),
              "new",
            ): Unit,
        )
        assert(failure.getMessage.contains("index page"))
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "old",
        )
      finally publication.close()

  test("a failed replacement rolls back every configured output directory"):
    withLayout: layout =>
      Files.createDirectories(layout.mdocDirectory)
      Files.createDirectories(layout.siteDirectory)
      Files.writeString(layout.mdocDirectory.resolve("index.md"), "old mdoc")
      Files.writeString(layout.siteDirectory.resolve("index.html"), "old site")
      var moveCount   = 0
      val publication = PreviewPublication.open(
        layout,
        logger,
        moveDirectory = (source, destination) =>
          moveCount += 1
          if moveCount == 4 then
            throw IllegalStateException("injected move failure")
          else Files.move(source, destination): Unit,
      )
      try
        intercept[IllegalStateException](
          publication.buildAndPublish(target => writeGeneration(target, "new")),
        ): Unit
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "old mdoc",
        )
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "old site",
        )
      finally publication.close()

  test("a later session recovers a backup after rollback itself fails"):
    withLayout: layout =>
      Files.createDirectories(layout.mdocDirectory)
      Files.createDirectories(layout.siteDirectory)
      Files.writeString(layout.mdocDirectory.resolve("index.md"), "old mdoc")
      Files.writeString(layout.siteDirectory.resolve("index.html"), "old site")
      var moveCount   = 0
      val compromised = PreviewPublication.open(
        layout,
        logger,
        moveDirectory = (source, destination) =>
          moveCount += 1
          if moveCount == 4 || moveCount == 5 then
            throw IllegalStateException(s"injected move failure $moveCount")
          else Files.move(source, destination): Unit,
      )
      try
        intercept[PreviewPublicationCompromised](
          compromised.buildAndPublish(target => writeGeneration(target, "new")),
        ): Unit
      finally compromised.close()

      val recovered = open(layout)
      try
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "old mdoc",
        )
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "old site",
        )
      finally recovered.close()

  test(
    "startup cleanup removes marked stale paths but preserves unowned siblings",
  ):
    withLayout: layout =>
      val staleMdoc = writeStaleTarget(layout.mdocDirectory)
      val staleSite = writeStaleTarget(layout.siteDirectory)
      Files.writeString(staleSite.resolve("partial"), "partial")
      val unowned = layout.siteDirectory.getParent.resolve(
        s".${layout.siteDirectory.getFileName}.doctrellis-${UUID.randomUUID()}.staging",
      )
      Files.createDirectories(unowned)

      val replacement = open(layout)
      try
        assert(!Files.exists(staleMdoc))
        assert(!Files.exists(staleSite))
        assert(Files.isDirectory(unowned))
      finally replacement.close()

  crashBoundaries.foreach: (description, completedMoves) =>
    test(s"uncommitted stale cleanup restores the old pair $description"):
      withLayout: layout =>
        val stale = writeStaleCrashState(layout, completedMoves)

        val firstRecovery = open(layout)
        try assertOldGeneration(layout)
        finally firstRecovery.close()
        stale.foreach: target =>
          assert(!Files.exists(target.staging))
          assert(!Files.exists(target.backup))
          assert(!Files.exists(target.marker))
        assertEquals(ownedPaths(layout), Vector.empty)

        val secondRecovery = open(layout)
        try assertOldGeneration(layout)
        finally secondRecovery.close()
        assertEquals(ownedPaths(layout), Vector.empty)

  test("startup cleanup preserves malformed and mismatched owner paths"):
    withLayout: layout =>
      val malformed = staleTargetPaths(
        layout.mdocDirectory,
        "index.md",
        UUID.randomUUID().toString,
      )
      val mismatched = staleTargetPaths(
        layout.siteDirectory,
        "index.html",
        UUID.randomUUID().toString,
      )
      Vector(malformed, mismatched).foreach: target =>
        Files.createDirectories(target.staging)
        Files.createDirectories(target.backup)
        Files.writeString(target.staging.resolve("sentinel"), "staging")
        Files.writeString(target.backup.resolve("sentinel"), "backup")
      Files.writeString(malformed.marker, "not a DocTrellis owner marker")
      Files.writeString(
        mismatched.marker,
        ownerContents(layout.mdocDirectory, mismatched.sessionId),
      )

      val replacement = open(layout)
      try
        Vector(malformed, mismatched).foreach: target =>
          assertEquals(
            Files.readString(target.staging.resolve("sentinel")),
            "staging",
          )
          assertEquals(
            Files.readString(target.backup.resolve("sentinel")),
            "backup",
          )
          assert(Files.isRegularFile(target.marker))
      finally replacement.close()

  test("startup cleanup preserves an undecodable owner and its paths"):
    withLayout: layout =>
      val stale = staleTargetPaths(
        layout.siteDirectory,
        "index.html",
        UUID.randomUUID().toString,
      )
      Files.createDirectories(stale.staging)
      Files.createDirectories(stale.backup)
      Files.writeString(stale.staging.resolve("sentinel"), "staging")
      Files.writeString(stale.backup.resolve("sentinel"), "backup")
      Files.write(stale.marker, invalidUtf8)
      val markerTemporary = ownerTemporary(stale.marker)
      Files.write(markerTemporary, invalidUtf8)

      val replacement = open(layout)
      try
        assertEquals(
          Files.readString(stale.staging.resolve("sentinel")),
          "staging",
        )
        assertEquals(
          Files.readString(stale.backup.resolve("sentinel")),
          "backup",
        )
        assert(Files.readAllBytes(stale.marker).sameElements(invalidUtf8))
        assert(
          Files.readAllBytes(markerTemporary).sameElements(invalidUtf8),
        )
      finally replacement.close()

  test("startup cleanup never follows or publishes owned symlinks"):
    assume(!isWindows, "requires Unix symbolic-link semantics")
    withLayout: layout =>
      Files.createDirectories(layout.siteDirectory)
      Files.writeString(layout.siteDirectory.resolve("index.html"), "current")
      val stale = staleTargetPaths(
        layout.siteDirectory,
        "index.html",
        UUID.randomUUID().toString,
      )
      val externalStaging = Files.createDirectories(
        layout.siteDirectory.getParent.resolve("external-staging"),
      )
      val externalBackup = Files.createDirectories(
        layout.siteDirectory.getParent.resolve("external-backup"),
      )
      Files.writeString(externalStaging.resolve("sentinel"), "staging")
      Files.writeString(externalBackup.resolve("index.html"), "external")
      Files.createSymbolicLink(stale.staging, externalStaging)
      Files.createSymbolicLink(stale.backup, externalBackup)
      Files.writeString(
        stale.marker,
        ownerContents(stale.destination, stale.sessionId),
      )

      val replacement = open(layout)
      try
        assert(!Files.isSymbolicLink(layout.siteDirectory))
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "current",
        )
        assert(!Files.exists(stale.staging))
        assert(!Files.exists(stale.backup))
        assertEquals(
          Files.readString(externalStaging.resolve("sentinel")),
          "staging",
        )
        assertEquals(
          Files.readString(externalBackup.resolve("index.html")),
          "external",
        )
      finally replacement.close()

  test("committed stale cleanup preserves the newer destination"):
    withLayout: layout =>
      val stale = writeStaleSwap(
        layout = layout,
        committed = true,
        current = "new site",
        backup = "old site",
      )

      val replacement = open(layout)
      try
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "new site",
        )
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "new site",
        )
        stale.backups.foreach(path => assert(!Files.exists(path)))
        stale.markers.foreach(path => assert(!Files.exists(path)))
        assert(!Files.exists(stale.transaction))
      finally replacement.close()

  test("uncommitted stale cleanup restores the previous destination"):
    withLayout: layout =>
      val stale = writeStaleSwap(
        layout = layout,
        committed = false,
        current = "uncommitted site",
        backup = "old site",
      )

      val replacement = open(layout)
      try
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "old site",
        )
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "old site",
        )
        stale.backups.foreach(path => assert(!Files.exists(path)))
        stale.markers.foreach(path => assert(!Files.exists(path)))
        assert(!Files.exists(stale.transaction))
      finally replacement.close()

  test("an undecodable owned commit is recovered as uncommitted"):
    withLayout: layout =>
      val stale = writeStaleSwap(
        layout = layout,
        committed = false,
        current = "uncommitted site",
        backup = "old site",
      )
      Files.write(stale.transaction, invalidUtf8)

      val replacement = open(layout)
      try
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "old site",
        )
        assertEquals(
          Files.readString(layout.mdocDirectory.resolve("index.md")),
          "old site",
        )
        assert(!Files.exists(stale.transaction))
      finally replacement.close()

  test("startup cleanup preserves an undecodable orphan commit"):
    withLayout: layout =>
      val transaction = transactionPath(layout, UUID.randomUUID().toString)
      Files.write(transaction, invalidUtf8)

      val replacement = open(layout)
      try
        assert(Files.readAllBytes(transaction).sameElements(invalidUtf8))
      finally replacement.close()
      assert(Files.readAllBytes(transaction).sameElements(invalidUtf8))

  test("a failed owner marker install cleans every owned temporary path"):
    withLayout: layout =>
      var replacements = 0
      val failure      = intercept[IllegalStateException](
        PreviewPublication.open(
          layout,
          logger,
          replaceMarker = (source, destination) =>
            replacements += 1
            if replacements == 2 then
              throw IllegalStateException("injected marker replacement failure")
            else Files.move(source, destination): Unit,
        ),
      )
      assertEquals(failure.getMessage, "injected marker replacement failure")
      assertEquals(replacements, 2)
      assertEquals(ownedPaths(layout), Vector.empty)

      val afterFailure = open(layout)
      afterFailure.close()
      assertEquals(ownedPaths(layout), Vector.empty)

  test("an uncommitted owner without a backup preserves the destination"):
    withLayout: layout =>
      Files.createDirectories(layout.siteDirectory)
      Files.writeString(layout.siteDirectory.resolve("index.html"), "current")
      val _ = writeStaleTarget(layout.siteDirectory)

      val replacement = open(layout)
      try
        assertEquals(
          Files.readString(layout.siteDirectory.resolve("index.html")),
          "current",
        )
      finally replacement.close()

  test("a concurrent preview cannot clean the live session staging paths"):
    withLayout: layout =>
      val first      = open(layout)
      val stagedMdoc = first.stagedTarget.mdocDirectory
      val stagedSite = first.stagedTarget.siteDirectory
      Files.createDirectories(stagedMdoc)
      Files.createDirectories(stagedSite)
      Files.writeString(stagedSite.resolve("partial"), "live")
      try
        val failure = intercept[IllegalStateException](open(layout))
        assert(failure.getMessage.contains("already publishing"))
        assert(Files.isDirectory(stagedMdoc))
        assertEquals(Files.readString(stagedSite.resolve("partial")), "live")
      finally first.close()

      val afterRelease = open(layout)
      afterRelease.close()

  private def writeGeneration(
      target: DocTrellisBuildTarget,
      content: String,
  ): Unit =
    Files.createDirectories(target.mdocDirectory)
    Files.createDirectories(target.siteDirectory)
    Files.writeString(target.mdocDirectory.resolve("index.md"), content)
    Files.writeString(target.siteDirectory.resolve("index.html"), content): Unit

  private def open(layout: DocTrellisLayout): PreviewPublication =
    PreviewPublication.open(layout, logger)

  private final case class StaleTargetPaths(
      destination: Path,
      indexName: String,
      staging: Path,
      backup: Path,
      marker: Path,
      sessionId: String,
  )

  private def staleTargetPaths(
      destination: Path,
      indexName: String,
      sessionId: String,
  ): StaleTargetPaths =
    val normalized = destination.toAbsolutePath.normalize()
    val prefix     = s".${normalized.getFileName}.doctrellis-$sessionId"
    StaleTargetPaths(
      destination = normalized,
      indexName = indexName,
      staging = normalized.getParent.resolve(s"$prefix.staging"),
      backup = normalized.getParent.resolve(s"$prefix.backup"),
      marker = normalized.getParent.resolve(s"$prefix.owner"),
      sessionId = sessionId,
    )

  private def writeStaleCrashState(
      layout: DocTrellisLayout,
      completedMoves: Int,
  ): Vector[StaleTargetPaths] =
    val sessionId  = UUID.randomUUID().toString
    val mdocTarget =
      staleTargetPaths(layout.mdocDirectory, "index.md", sessionId)
    val siteTarget =
      staleTargetPaths(layout.siteDirectory, "index.html", sessionId)
    val targets = Vector(mdocTarget, siteTarget)
    targets.foreach: target =>
      Files.createDirectories(target.destination)
      Files.writeString(target.destination.resolve(target.indexName), "old")
      Files.createDirectories(target.staging)
      Files.writeString(target.staging.resolve(target.indexName), "new")
      Files.writeString(
        target.marker,
        ownerContents(target.destination, target.sessionId),
      )
    val moves = Vector[() => Unit](
      () => Files.move(mdocTarget.destination, mdocTarget.backup): Unit,
      () => Files.move(siteTarget.destination, siteTarget.backup): Unit,
      () => Files.move(mdocTarget.staging, mdocTarget.destination): Unit,
      () => Files.move(siteTarget.staging, siteTarget.destination): Unit,
    )
    moves.take(completedMoves).foreach(move => move())
    targets

  private def assertOldGeneration(layout: DocTrellisLayout): Unit =
    assertEquals(
      Files.readString(layout.mdocDirectory.resolve("index.md")),
      "old",
    )
    assertEquals(
      Files.readString(layout.siteDirectory.resolve("index.html")),
      "old",
    )

  private def ownerContents(destination: Path, sessionId: String): String =
    val normalized = destination.toAbsolutePath.normalize()
    s"doctrellis-preview-publication-v1\nsession=$sessionId\ntarget=$normalized\n"

  private def ownerTemporary(marker: Path): Path =
    marker.resolveSibling(s".${marker.getFileName}.tmp")

  private def writeStaleTarget(destination: Path): Path =
    val normalized = destination.toAbsolutePath.normalize()
    val sessionId  = UUID.randomUUID().toString
    val prefix     = s".${normalized.getFileName}.doctrellis-$sessionId"
    val staging    = normalized.getParent.resolve(s"$prefix.staging")
    val marker     = normalized.getParent.resolve(s"$prefix.owner")
    Files.createDirectories(staging)
    Files.writeString(
      marker,
      s"doctrellis-preview-publication-v1\nsession=$sessionId\ntarget=$normalized\n",
    )
    staging

  private final case class StaleSwap(
      backups: Vector[Path],
      markers: Vector[Path],
      transaction: Path,
  )

  private def writeStaleSwap(
      layout: DocTrellisLayout,
      committed: Boolean,
      current: String,
      backup: String,
  ): StaleSwap =
    val sessionId = UUID.randomUUID().toString
    val targets   = Vector(
      layout.mdocDirectory.toAbsolutePath.normalize() -> "index.md",
      layout.siteDirectory.toAbsolutePath.normalize() -> "index.html",
    )
    val owned = targets.map: (destination, indexName) =>
      val prefix     = s".${destination.getFileName}.doctrellis-$sessionId"
      val backupPath = destination.getParent.resolve(s"$prefix.backup")
      val marker     = destination.getParent.resolve(s"$prefix.owner")
      Files.createDirectories(destination)
      Files.writeString(destination.resolve(indexName), current)
      Files.createDirectories(backupPath)
      Files.writeString(backupPath.resolve(indexName), backup)
      Files.writeString(
        marker,
        s"doctrellis-preview-publication-v1\nsession=$sessionId\ntarget=$destination\n",
      )
      backupPath -> marker
    val site        = targets.last._1
    val prefix      = s".${site.getFileName}.doctrellis-$sessionId"
    val transaction = site.getParent.resolve(s"$prefix.commit")
    if committed then
      val targetLines = targets.zipWithIndex
        .map((target, index) => s"target.$index=${target._1}")
        .mkString("\n")
      Files.writeString(
        transaction,
        s"doctrellis-preview-publication-v1\nsession=$sessionId\n$targetLines\nstate=committed\n",
      ): Unit
    StaleSwap(
      backups = owned.map(_._1),
      markers = owned.map(_._2),
      transaction = transaction,
    )

  private def transactionPath(
      layout: DocTrellisLayout,
      sessionId: String,
  ): Path =
    val site   = layout.siteDirectory.toAbsolutePath.normalize()
    val prefix = s".${site.getFileName}.doctrellis-$sessionId"
    site.getParent.resolve(s"$prefix.commit")

  private def logger = LogExchange.logger("preview-publication-test")

  private def ownedPaths(layout: DocTrellisLayout): Vector[Path] =
    val parents = Vector(
      layout.mdocDirectory.getParent,
      layout.siteDirectory.getParent,
    ).distinct
    parents.flatMap: parent =>
      Using.resource(Files.list(parent)): paths =>
        paths
          .iterator()
          .asScala
          .toVector
          .filter(path =>
            val name = path.getFileName.toString
            name.endsWith(".owner") || name.endsWith(".staging") ||
            name.endsWith(".backup") || name.endsWith(".commit") ||
            name.endsWith(".owner.tmp") || name.endsWith(".commit.tmp"),
          )

  private def transactionPaths(layout: DocTrellisLayout): Vector[Path] =
    ownedPaths(layout).filter(_.getFileName.toString.endsWith(".commit"))

  private def withLayout(action: DocTrellisLayout => Unit): Unit =
    val root      = Files.createTempDirectory("doctrellis-publication-")
    val workspace = Files.createDirectories(root.resolve("workspace"))
    val source    = Files.createDirectories(workspace.resolve("docs"))
    val output    = Files.createDirectories(root.resolve("output"))
    val layout    = DocTrellisLayout
      .parse(
        workspaceDirectory = workspace,
        sourceDirectory = source,
        mdocDirectory = output.resolve("mdoc"),
        siteDirectory = output.resolve("site"),
      )
      .fold(problem => fail(problem.message), identity)
    try action(layout)
    finally GeneratedDirectory.delete(root)

  private def isWindows: Boolean =
    System
      .getProperty("os.name", "")
      .toLowerCase(java.util.Locale.ROOT)
      .contains("win")
