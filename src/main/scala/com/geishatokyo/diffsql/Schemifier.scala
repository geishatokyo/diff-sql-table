package com.geishatokyo.diffsql

import scala.slick.session.Database
import Database.threadLocalSession
import scala.slick.jdbc.{GetResult, StaticQuery => Q}
import scala.slick.jdbc.StaticQuery.interpolation

import slick.driver.MySQLDriver.simple._

/**
*
* User: takeshita
* DateTime: 13/05/29 16:11
*/
class SlickSchemifier(db : Database) {


  def log(string : String) = println(string)
  def logSql(sql : String) = println(sql)

  var permitAlterTable = true

  private def showTables : List[String] = {
    Q.query[Unit,String]("SHOW TABLES;").list()
  }

  private def showCreateTable(tableName : String) : Option[String] = {
    Q.query[Unit,(String,String)]("SHOW CREATE TABLE " + tableName + ";").list().headOption.map(_._2)
  }

  def dropAllTables() = {
    db.withSession{
      showTables.foreach(tn => {
        Q.update[Unit]("DROP TABLE IF EXISTS " + tn + ";").execute()
      })
    }
  }

  def schemify( _tables : AnyRef*) = {

    val tables = _tables.map(_.asInstanceOf[slick.driver.MySQLDriver.simple.Table[_]])

    db.withSession{
      val tablesOnDb = showTables.map(_.toLowerCase)

      val (exists, notExists) = tables.partition(t => tablesOnDb.contains(t.tableName.toLowerCase))
      for (t <- notExists){
        log("Create table " + t.tableName)
        t.ddl.createStatements.foreach(s => {
          logSql(s)
        })
        t.ddl.create
      }
      if(permitAlterTable){
        for {
          t <- exists
          onDb = showCreateTable(t.tableName).get
          newTable = t.ddl.createStatements.next()
          diff = SqlParser.diff(onDb,newTable)
          if diff.isSuccess
        } {
          log("Diff:" + diff)

          log("Alter talbe " + t.tableName)

          Q.query[Unit,String](diff.get.toString).execute()

        }

        SchemifyResult(notExists.map(_.tableName).toList,exists.map(_.tableName).toList)
      }else{
        SchemifyResult(notExists.map(_.tableName).toList,Nil)
      }


    }


  }




}

case class SchemifyResult(creates : List[String], alters : List[String])
