import sbt._
import sbt.Keys._

import scala.xml.XML

object DiffSqlBuild extends Build {

  val defaultSettings = Project.defaultSettings ++ Seq(
    organization := "com.geishatokyo.tools",
    version := "0.3.5",
    scalaVersion := "2.11.7",
    crossScalaVersions := Seq("2.11.7","2.10.4"),
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

  def publishToMaven = Command.command("publishToMaven")( state => {

    val switched = Command.process("project diff-sql-table-parser",state)

    val ext = Project.extract(switched)
    val nexus = "https://oss.sonatype.org/"
    val snapShot_? = ext.get(isSnapshot)

    // 芸者東京では、global.sbtなどに社内レポジトリを登録しており、その設定と干渉するため、
    // このように後からデプロイ設定を更新しています。
    val resolver = if(snapShot_?){
      Some("snapshots"  at nexus + "content/repositories/snapshots")
    }else{
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    }
    val publishState = ext.append(Seq(
      publishTo := resolver
    ),switched)

    val lastState = Command.process("publishSigned",publishState)

    state
  })

  lazy val root = Project(
    id = "diff-sql-table",
    base = file("."),
    settings = defaultSettings ++ Seq(
      commands += publishToMaven
    )
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
      pomExtra := pom,
      publishTo := {
        val nexus = "https://oss.sonatype.org/"
        if (isSnapshot.value)
          Some("snapshots" at nexus + "content/repositories/snapshots")
        else
          Some("releases"  at nexus + "service/local/staging/deploy/maven2")
      }
    )
  )


  def pom = {
    <url>https://github.com/geishatokyo/diff-sql-table</url>
    <licenses>
      <license>
        <name>MIT License</name>
        <url>http://www.opensource.org/licenses/mit-license.php</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:git@github.com:geishatokyo/diff-sql-table.git</connection>
      <developerConnection>scm:git:git@github.com:geishatokyo/diff-sql-table.git</developerConnection>
      <url>scm:git:git@github.com:geishatokyo/diff-sql-table.git</url>
    </scm>

    <developers>
      <developer>
        <id>takeshita</id>
        <name>Yositeru Takeshita</name>
        <email>takezoux2@gmail.com</email>
      </developer>
    </developers>
  }

  lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.1" % "test"

}
