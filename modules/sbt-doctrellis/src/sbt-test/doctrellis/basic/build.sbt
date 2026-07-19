import org.doctrellis.sbtplugin.DocTrellisPlugin.autoImport.*

val writeScriptedPluginVersion = taskKey[Unit](
  "Write the plugin version supplied by scripted for the nested preview build",
)

organization := "org.doctrellis.example"
name := "doctrellis-scripted-basic"
scalaVersion := "3.3.8"

enablePlugins(org.doctrellis.sbtplugin.DocTrellisPlugin)

writeScriptedPluginVersion := {
  val pluginVersion = sys.props.getOrElse(
    "plugin.version",
    sys.error("scripted did not supply plugin.version"),
  )
  IO.write(baseDirectory.value / "scripted-plugin.version", s"$pluginVersion\n")
}
