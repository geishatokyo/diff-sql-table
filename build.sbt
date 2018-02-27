val commonSettings = Seq(
  organization := "com.geishatokyo.tools",
  version := "0.3.6-SNAPSHOT",
  scalaVersion := "2.12.4",
  crossScalaVersions := Seq("2.12.4","2.11.12"),
  scalacOptions ++= Seq("-feature")
)

lazy val root = (project in file(".")).settings(commonSettings:_*).settings(
  name := "diff-sql-table"
) aggregate(parser)


lazy val parser = (project in file("parser")).settings(commonSettings:_*).settings(
  name := "diff-sql-table-parser",
  libraryDependencies ++= Seq(
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.0"
  )
)