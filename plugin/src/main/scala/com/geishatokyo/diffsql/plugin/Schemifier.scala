package com.geishatokyo.diffsql.plugin

import sbt._, Keys._

import scala.slick.session.Database

import slick.driver.MySQLDriver.simple._

import com.geishatokyo.diffsql.SqlParser

object Schemifier extends Plugin {

  lazy val diffSqlUrl = SettingKey[String]("db-url")
  lazy val diffSqlDriver = SettingKey[String]("db-driver")
  lazy val diffSqlAlter = TaskKey[Unit]("alter-table")

  override lazy val settings = Seq(
    diffSqlAlter <<= (diffSqlUrl, diffSqlDriver) map { (url, driver) =>
      Database.forURL(url, driver=driver) withSession {
      }
    }
  )

}
