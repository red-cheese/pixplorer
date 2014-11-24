name := """pixplorer"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.1"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws
)


libraryDependencies += "net.ruippeixotog" %% "scala-scraper" % "0.1.1"

libraryDependencies += "net.databinder.dispatch" %% "dispatch-core" % "0.11.2"