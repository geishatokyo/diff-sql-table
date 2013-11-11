import sbt._
import sbt.Keys._

object DiffSqlBuild extends Build {

  val defaultSettings = Project.defaultSettings ++ Seq(
    organization := "com.geishatokyo.tools",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.3",
    crossScalaVersions := Seq("2.9.2","2.10.3"),
    scalacOptions <++= (scalaVersion) map { v =>
      if (v startsWith "2.9")
        Seq()
      else
        Seq(
          "-feature",
          "-language:postfixOps",
          "-language:implicitConversions",
          "-language:reflectiveCalls"
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
      libraryDependencies += scalatest,
      libraryDependencies <++= (scalaVersion)(sv => {
        if(sv.startsWith("2.10")){
          Seq("org.scala-lang" % "scala-reflect" % sv)
        }else{
          Seq()
        }
      })
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
