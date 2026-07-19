import org.scalajs.linker.interface.{ModuleKind, ModuleSplitStyle}
import sbt.AutoPlugin

ThisBuild / organization      := "org.doctrellis"
ThisBuild / version           := "0.1.0-SNAPSHOT"
ThisBuild / homepage          := Some(uri("https://doctrellis.org"))
ThisBuild / semanticdbEnabled := true

private def selfHostedDocTrellisPlugin: Seq[AutoPlugin] =
  if (sys.props.get("doctrellis.selfHost").contains("true")) {
    // A fresh checkout must publish the plugin before loading it as a build
    // dependency. This boundary is therefore only resolved by the explicit
    // self-host Preview process, never by ordinary builds or CI.
    val module = Class
      .forName("org.doctrellis.sbtplugin.DocTrellisPlugin$")
      .getField("MODULE$")
      .get(null)
    Seq(module.asInstanceOf[AutoPlugin])
  } else Seq.empty

lazy val root = project
  .in(file("."))
  .aggregate(sbtDoctrellis, vscodeDomain, vscodeApplication, vscode)
  .enablePlugins(selfHostedDocTrellisPlugin *)
  .settings(
    name           := "doctrellis",
    publish / skip := true,
  )

lazy val sbtDoctrellis = project
  .in(file("modules/sbt-doctrellis"))
  .enablePlugins(SbtPlugin)
  .settings(
    name        := "sbt-doctrellis",
    description := "Build and locally preview mdoc-powered Laika documentation",
    pluginCrossBuild / sbtVersion := "2.0.3",
    addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.9.1"),
    libraryDependencies ++= Seq(
      "org.scalameta"  % "mdoc-interfaces" % "2.9.1",
      "org.typelevel" %% "laika-io"        % "1.3.2",
      "org.scalameta" %% "munit"           % "1.3.4" % Test,
    ),
    Compile / compile / wartremoverErrors ++= doctrellisWarts,
    scriptedLaunchOpts ++= Seq(
      "-Xmx1024M",
      s"-Dplugin.version=${version.value}",
    ),
    scriptedBufferLog := false,
  )

lazy val vscodeDomain = project
  .in(file("modules/vscode/domain"))
  .enablePlugins(ScalaJSPlugin)
  .settings(
    name           := "doctrellis-vscode-domain",
    scalaVersion   := "3.3.8",
    publish / skip := true,
    Test / fork    := false,
    Compile / compile / wartremoverErrors ++= doctrellisWarts,
    // sbt 2 uses the project `platform` setting, so %% adds the _sjs1 suffix.
    libraryDependencies += "org.scalameta" %% "munit" % "1.3.4" % Test,
  )

lazy val vscodeApplication = project
  .in(file("modules/vscode/application"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(vscodeDomain)
  .settings(
    name           := "doctrellis-vscode-application",
    scalaVersion   := "3.3.8",
    publish / skip := true,
    Test / fork    := false,
    Compile / compile / wartremoverErrors ++= doctrellisWarts,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"       % "3.7.0",
      "org.typelevel" %% "munit-cats-effect" % "2.2.0" % Test,
    ),
  )

lazy val vscode = project
  .in(file("modules/vscode/extension"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(vscodeApplication)
  .settings(
    name                            := "doctrellis-vscode",
    scalaVersion                    := "3.3.8",
    publish / skip                  := true,
    Test / fork                     := false,
    moduleName                      := "extension",
    scalaJSUseMainModuleInitializer := false,
    scalaJSLinkerConfig             := scalaJSLinkerConfig.value
      .withModuleKind(ModuleKind.CommonJSModule)
      .withModuleSplitStyle(ModuleSplitStyle.FewestModules)
      .withSourceMap(true),
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value.getParentFile / "dist",
    Compile / fullLinkJS / scalaJSLinkerOutputDirectory := baseDirectory.value.getParentFile / "dist",
    Compile / compile / wartremoverErrors ++= doctrellisWarts,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-effect"       % "3.7.0",
      "org.typelevel" %% "munit-cats-effect" % "2.2.0" % Test,
    ),
  )

lazy val doctrellisWarts = Seq(
  Wart.ArrayEquals,
  Wart.EitherProjectionPartial,
  Wart.IsInstanceOf,
  Wart.LeakingSealed,
  Wart.OptionPartial,
  Wart.Return,
  Wart.TryPartial,
)
