package com.geishatokyo.diffsql.mysql

import com.geishatokyo.diffsql.{Name, SQLnizer}
import com.geishatokyo.diffsql.ast.Key._
import com.geishatokyo.diffsql.ast.{Column, Table, _}
import com.geishatokyo.diffsql.diff.Diff

/**
 * Created by takeshita on 14/02/17.
 */
class MySQLnizer extends SQLnizer {

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
    diff.columns.remove.map( c => {
      s"ALTER TABLE ${diff.tableName} DROP COLUMN ${c.name.name};"
    }) :::
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


  def toIndexDefinition(key : Key) = {
    val prefix = key match{
      case _ : PrimaryKey => "PRIMARY KEY"
      case _ : UniqueKey => "UNIQUE KEY"
      case _ : NormalKey => "KEY"
      case _ : FullTextKey => "FULLTEXT"
      case _ : ForeignKey => "FOREIGN KEY"
    }

    val names = key.name.map(" " + _.name).getOrElse("")
    val indexType = key.algorithm.map(" USING " + _).getOrElse("")
    val columns = columnsToString(key.columns)
    val order = key.order.map(_.toString).getOrElse("")

    val base = s"${prefix}${names}${indexType} ${columns} ${order}"
    key match {
      case fk: ForeignKey => s"${base} ${referenceToString(fk.reference)}"
      case _ => base
    }
  }

  private def referenceToString(ref: Reference): String = {
    val columns = columnsToString(ref.columns)
    val onDelete = ref.onDelete.map { it =>
      s" ON DELETE ${ReferenceOption.toString(it)}"
    }.getOrElse("")
    val onUpdate = ref.onUpdate.map { it =>
      s" ON UPDATE ${ReferenceOption.toString(it)}"
    }.getOrElse("")
    s"REFERENCES ${ref.table}${columns}${onDelete}${onUpdate}"
  }

  private def columnsToString(cols: List[Name]): String = cols.map(_.name).mkString("(", ",", ")")
}
