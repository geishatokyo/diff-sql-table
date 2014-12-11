import sbt._
import sbt.Keys._

import scala.xml.XML

object DiffSqlBuild extends Build {

  val defaultSettings = Project.defaultSettings ++ Seq(
    organization := "com.geishatokyo.tools",
    version := "0.3.5-SNAPSHOT",
    scalaVersion := "2.11.2",
    crossScalaVersions := Seq("2.11.2","2.10.4"),
    scalacOptions <++= (scalaVersion) map { v =>
      if (v startsWith "2.9")
        Seq()
      else
        Seq(
          "-feature",
          "-language:postfixOps",
          "-language:implicitConversions",
          "-language:reflectiveCalls",
          "-deprecation"
        )
    }
  )

  lazy val root = Project(
    id = "diff-sql-table",
    base = file("."),
    settings = defaultSettings
  ) aggregate(parser)

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
      }),
      publishMavenStyle := true,
      pomIncludeRepository := { _ => false },
      pomExtra := loadPomExtra()
    )
  )

  /**
   *
   * @return
   */
  def loadPomExtra() = {
    XML.loadFile( file( "./project/pomExtra.xml")).child
  }

  lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.1" % "test"

}
