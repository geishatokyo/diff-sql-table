organization := "com.geishatokyo.tools"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.2"

scalacOptions <++= (scalaVersion) map { v =>
  if (v startsWith "2.9")
    Seq()
  else
    Seq("-feature", "-language:postfixOps", "-language:implicitConversions")
}


crossScalaVersions := Seq("2.9.2", "2.10.2")

libraryDependencies += "org.scalatest" %% "scalatest" % "1.9.1" % "test"
