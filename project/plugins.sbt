addSbtPlugin("ch.epfl.scala"   % "sbt-scalafix"    % "0.14.7")
addSbtPlugin("org.scala-js"    % "sbt-scalajs"     % "1.22.0")
addSbtPlugin("org.wartremover" % "sbt-wartremover" % "3.6.1")
addSbtPlugin("org.scalameta"   % "sbt-scalafmt"    % "2.6.2")

if (sys.props.get("doctrellis.selfHost").contains("true")) {
  addSbtPlugin("org.doctrellis" % "sbt-doctrellis" % "0.1.0-SNAPSHOT")
} else {
  libraryDependencies ++= Seq.empty
}
