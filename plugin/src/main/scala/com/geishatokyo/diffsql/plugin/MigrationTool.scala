package com.geishatokyo.diffsql.plugin

import sbt._, Keys._

import scala.io.Source

import java.text.{DateFormat,SimpleDateFormat}

import com.geishatokyo.diffsql.{SqlParser,Result}

object MigrationTool extends Plugin {

  type Migration = (Set[Result], Set[Result]) => Unit

  lazy val sqlDirs =
    SettingKey[Seq[File]]("sql-directiories")
  lazy val dateFormat =
    SettingKey[DateFormat]("file-date-format")
  lazy val migration =
    SettingKey[Migration]("migration")

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
    (sqlDirs, migration, dateFormat) { (dirs, m, format) =>
      Command.command("gen-migration-file") { state =>
        val out = file(format.format(new java.util.Date))
        fileWith(dirs, format) { (n, o) =>
          m(SqlParser.genSql(n, o), SqlParser.genSql(o, n))
        }
        state
      }
    }


  lazy val migrationToolSettings: Seq[Setting[_]] = Seq(
    dateFormat := new SimpleDateFormat("yyyyMMddHHmmss"),
    sqlDirs := Nil,
    migration := ({ (x, y) =>
      println(x)
      println(y)
    }: Migration),
    commands <++= Seq(migrate, rollback, genMigration).join
  )

}
