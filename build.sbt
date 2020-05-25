name := "blurhash-scala"

organization := "mrks"

scalaVersion := "2.13.1"

crossScalaVersions := Seq("2.12.10", "2.13.1")

licenses += ("Apache-2.0", url("https://github.com/markussammallahti/blurhash-scala/blob/master/LICENSE"))

scalacOptions ++= Seq(
  "-encoding", "utf-8",
  "-deprecation",
  "-unchecked",
  "-feature",
  "-Xfatal-warnings",
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "com.github.pathikrit" %% "better-files" % "3.8.0" % Test
)
