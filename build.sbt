// Official sbt documentation at http://www.scala-sbt.org/documentation.html

// Scala settings

scalaVersion := "2.13.12"

// Project settings

name := "SmallSearch"
organization := "tech.talo"
version := "0.0.1"

// Dependencies

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4", // For ParSeq
  "org.scalatest" %% "scalatest" % "3.2.15" % Test // For testing
)

// Commands
lazy val devRun = Command.command("dev-run") { state =>
  "clean" ::
  "compile" ::
  "run" ::
  state
}

commands ++= Seq(
  devRun
)
