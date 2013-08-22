organization := "com.geishatokyo.tools"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

scalacOptions ++= Seq(
  "-feature",
  "-language:implicitConversions",
  "-language:postfixOps"
)

libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "1.0.1" % "test",
  "org.scalatest" %% "scalatest" % "1.9.1" % "test"
)
