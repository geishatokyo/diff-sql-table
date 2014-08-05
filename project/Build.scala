import sbt._
import sbt.Keys._

object DiffSqlBuild extends Build {

  val defaultSettings = Project.defaultSettings ++ Seq(
    organization := "com.geishatokyo.tools",
    version := "0.3.1-SNAPSHOT",
    scalaVersion := "2.11.1",
    crossScalaVersions := Seq("2.11.1","2.10.4"),
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
        if(sv.startsWith("2.11")){
          Seq(
            "org.scala-lang" % "scala-reflect" % sv,
            "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
          )
        }else if(sv.startsWith("2.10")) {
          Seq("org.scala-lang" % "scala-reflect" % sv)
        }else {
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

  lazy val scalatest = "org.scalatest" %% "scalatest" % "2.1.7" % "test"

}
