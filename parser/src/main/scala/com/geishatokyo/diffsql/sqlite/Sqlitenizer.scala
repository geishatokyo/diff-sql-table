package com.geishatokyo.diffsql.sqlite

import com.geishatokyo.diffsql.{ SQLnizer}
import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.diff.Diff
import com.geishatokyo.diffsql.ast.Key._
import com.geishatokyo.diffsql.ast.Table
import com.geishatokyo.diffsql.diff.Diff
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/17.
 */
class Sqlitenizer extends SQLnizer {

  def toCreateTable(table: Table): String = {
    s"""CREATE TABLE IF NOT EXISTS ${table.name.name} (
  ${table.fields.map({
      case c : Column => toColumnDefinition(c)
      case k : Key => toIndexDefinition(k)
    }).mkString(",\n  ")}
)${table.options.mkString(",\n")};"""

  }

  def toDropTable(table: Table) : String = {
    s"DROP TABLE IF EXISTS ${table.name.name};"
  }


  def toAlterSQL(diff: Diff): List[String] = {
    columns(diff) ::: keys(diff)
  }

  def columns(diff : Diff) = {
    diff.columns.add.map( c => {
      s"ALTER TABLE ${diff.tableName} ADD COLUMN ${toColumnDefinition(c)};"
    }) :::
    // Drop column is not supported in Sqlite.
    /*diff.columns.remove.map( c => {
      s"ALTER TABLE ${diff.tableName} DROP COLUMN ${c.name.name};"
    }) :::*/
    diff.columns.alter.map(c => {
      s"ALTER TABLE ${diff.tableName} MODIFY ${toColumnDefinition(c)};"
    })
  }

  def toColumnDefinition(column: Column): String = {
    s"${column.name} ${column.dataType} ${column.options.mkString(" ")}"
  }

  def keys(diff : Diff) = {

    diff.keys.remove.map(k => {
      if(k.keyType == KeyType.PrimaryKey){
        s"ALTER TABLE ${diff.tableName} DROP PRIMARY KEY;"
      }else{
        s"ALTER TABLE ${diff.tableName} DROP KEY ${k.name.get.name};"
      }
    }) :::
    diff.keys.add.map(k => {
      s"ALTER TABLE ${diff.tableName} ADD ${toIndexDefinition(k)};"
    })

  }


  def toIndexDefinition( key : Key) = {

    val prefix = key match{
      case pk : PrimaryKey => "PRIMARY KEY"
      case uk : UniqueKey => "UNIQUE KEY"
      case nk : NormalKey => "KEY"
      case fk : FullTextKey => "FULLTEXT"
    }

    s"${prefix}${key.name.map(" " + _.name).getOrElse("")}${key.algorithm.map(" USING " + _).getOrElse("")} ${key.columns.map(_.name).mkString("(",",",")")} ${key.order.map(_.toString).getOrElse("")}"

  }

}
