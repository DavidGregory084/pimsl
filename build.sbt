lazy val root = (project in file(".")).
  settings(scalariformSettings: _*).
  settings(
    name := "pimsl",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.6",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "2.2.4" % "test",
      "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"
  ))

scalacOptions ++= Seq("-unchecked", "-feature")

resolvers += "bintray/non" at "http://dl.bintray.com/non/maven"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.6.0")