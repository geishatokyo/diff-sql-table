package com.geishatokyo.diffsql.plugin

import sbt._, Keys._

import scala.io.Source

import java.text.{DateFormat,SimpleDateFormat}

import com.geishatokyo.diffsql.{SqlParser,Result}

object MigrationTool extends Plugin {

  type Template = (Set[Result], Set[Result]) => String

  lazy val sqlDirs =
    SettingKey[Seq[File]]("sql-directiories")
  lazy val dateFormat =
    SettingKey[DateFormat]("file-date-format")
  lazy val migrationTemplate =
    SettingKey[Template]("migration-template")

  def fileWith(dirs: Seq[File], format: DateFormat)(f: (String, String) => Unit) =
    for (dir <- dirs) {
      val files = dir
        .listFiles
        .toList
        .sortBy(f => format.parse(f.getName))
        .reverse
      files match {
        case n :: o :: _ => f(IO.read(n), IO.read(o))
        case _ =>
          throw new RuntimeException("file not found")
      }
    }

  def genSql(name: String, f: (String, String) => Set[Result]) =
    (sqlDirs, dateFormat) { (dirs, format) =>
      Command.command(name) { state =>
        fileWith(dirs, format) { (n, o) =>
          println(f(n, o).mkString)
        }
        state
      }
    }

  lazy val migrate = genSql("migrate", SqlParser.genSql)
  lazy val rollback =
    genSql("rollback", (a, b) => SqlParser.genSql(b, a))

  lazy val genMigration =
    (sqlDirs, migrationTemplate, dateFormat) { (dirs, template, format) =>
      Command.command("gen-migration-file") { state =>
        val out = file(format.format(new java.util.Date))
        fileWith(dirs, format) { (n, o) =>
          println(template(
            SqlParser.genSql(n, o),
            SqlParser.genSql(o, n)))
        }
        state
      }
    }


  lazy val migrationToolSettings: Seq[Setting[_]] = Seq(
    dateFormat := new SimpleDateFormat("yyyyMMddHHmmss"),
    sqlDirs := Nil,
    commands <++= Seq(migrate, rollback, genMigration).join
  )

}
