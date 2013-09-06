package com.geishatokyo.diffsql.plugin

import sbt._, Keys._

import scala.io.Source

import java.text.{DateFormat,SimpleDateFormat}

import com.geishatokyo.diffsql.SqlParser

object MigrationTool extends Plugin {

  lazy val sqlDir = SettingKey[File]("sql-directiory")
  lazy val dateFormat = SettingKey[DateFormat]("file-date-format")

  lazy val migrate = (sqlDir, dateFormat) { (dir, format) =>
    Command.command("migration") { state =>
      val files = dir
        .listFiles
        .toList
        .sortBy(f => format.parse(f.getName))
        .reverse
      files match {
        case n :: o :: _ =>
          SqlParser.genSql(
            Source.fromFile(o).mkString,
            Source.fromFile(n).mkString).fold({ msg =>
              throw new RuntimeException(msg)
            }, _ foreach println)
            state
        case _ => throw new RuntimeException("file not found")
      }
    }
  }

  lazy val migrationToolSettings: Seq[Setting[_]] = Seq(
    dateFormat := new SimpleDateFormat("yyyyMMddHHmmss"),
    commands <+= migrate
  )

}
