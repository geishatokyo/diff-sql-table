import sbt._
import sbt.Keys._

object DiffSqlBuild extends Build {

  val defaultSettings = Project.defaultSettings ++ Seq(
    organization := "com.geishatokyo.tools",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq(
      "-feature",
      "-language:implicitConversions",
      "-language:postfixOps"
    )
  )

  lazy val root = Project(
    id = "diff-sql-table",
    base = file("."),
    settings = defaultSettings
  ) aggregate(parser, plugin)

  lazy val parser = Project(
    id = "diff-sql-table-parser",
    base = file("parser"),
    settings = defaultSettings ++ Seq(
      libraryDependencies ++= Seq(slick % "test", scalatest)
    )
  )

  lazy val plugin = Project(
    id = "diff-sql-table-plugin",
    base = file("plugin"),
    settings = defaultSettings ++ Seq(
      sbtPlugin := true,
      libraryDependencies += slick
    )
  ) dependsOn parser

  lazy val slick = "com.typesafe.slick" %% "slick" % "1.0.1"
  lazy val scalatest = "org.scalatest" %% "scalatest" % "1.9.1" % "test"

}
