import sbt._
import sbt.Keys._

object DiffSqlBuild extends Build {

  val defaultSettings = Project.defaultSettings ++ Seq(
    organization := "com.geishatokyo.tools",
    version := "0.1-SNAPSHOT",
    scalacOptions <++= (scalaVersion) map { v =>
      if (v startsWith "2.9")
        Seq()
      else
        Seq(
          "-feature",
          "-language:postfixOps",
          "-language:implicitConversions"
        )
    }
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
      scalaVersion := "2.9.2",
      libraryDependencies += scalatest
    )
  )

  lazy val plugin = Project(
    id = "diff-sql-table-plugin",
    base = file("plugin"),
    settings = defaultSettings ++ Seq(
      sbtPlugin := true
    )
  ) dependsOn parser

  lazy val scalatest = "org.scalatest" %% "scalatest" % "1.9.1" % "test"

}
