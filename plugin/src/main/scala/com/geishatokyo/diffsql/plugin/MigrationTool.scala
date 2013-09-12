package com.geishatokyo.diffsql.plugin

import sbt._, Keys._

import scala.io.Source

import java.text.{DateFormat,SimpleDateFormat}

import com.geishatokyo.diffsql.SqlParser

object MigrationTool extends Plugin {

  lazy val sqlDir = SettingKey[File]("sql-directiory")
  lazy val dateFormat = SettingKey[DateFormat]("file-date-format")
  lazy val template = SettingKey[Set[String] => String]("migration-template")

  lazy val migrate = (sqlDir, template, dateFormat) { (dir, template, format) =>
    Command.single("migrate") { (state, arg) =>
      val out = file(format.format(new java.util.Date))
      val write = arg match {
        case "print" => println(_: String)
        case "file" => IO.write(out, _: String)
      }
      val files = dir
        .listFiles
        .toList
        .sortBy(f => format.parse(f.getName))
        .reverse
      files match {
        case n :: o :: _ =>
          write(template(SqlParser.genSql(IO.read(n), IO.read(o)).fold(msg => throw new RuntimeException(msg), identity)))
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
