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
)${table.options.map(optionToSQL(_)).mkString(",\n")};"""

  }

  def toDropTable(table: Table) : String = {
    s"DROP TABLE IF EXISTS ${table.name.name};"
  }


  def toAlterSQL(diff: Diff): List[String] = {
    columns(diff) ::: keys(diff) ::: partitions(diff)
  }

  def optionToSQL(tableOp: TableOption) = {
    tableOp match{
      case p : Partition => toPartitionBlock(p)
      case o => o.toString
    }
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
    diff.keys.remove.map { k =>
      val special = k.keyType match {
        case KeyType.PrimaryKey => s"DROP PRIMARY KEY"
        case KeyType.ForeignKey => s"DROP FOREIGN KEY ${k.name.get.name}"
        case _ => s"DROP KEY ${k.name.get.name}"
      }
      s"ALTER TABLE ${diff.tableName} ${special};"
    } :::
    diff.keys.add.map { k =>
      s"ALTER TABLE ${diff.tableName} ADD ${toIndexDefinition(k)};"
    }
  }

  def partitions(diff: Diff) = {

    diff.tableOptions.add.collectFirst({
      case p : Partition => p
    }).map(o => {
      toCreatePartition(diff.tableName,o)
    }) orElse diff.tableOptions.alter.collectFirst({
      case p : Partition => p
    }).map(o => {
      toCreatePartition(diff.tableName,o)
    }) map(List(_)) getOrElse Nil
  }

  def toCreatePartition(table: String,partition: Partition) = {
    s"ALTER TABLE ${table} ${toPartitionBlock(partition)};"
  }
  def toPartitionBlock(partition: Partition) = {

    def convertLessThan(lt: LessThanRange) = {
      lt match{
        case maxValue : LessThanMaxValue => {
          s"PARTITION ${lt.name} VALUES LESS THAN MAXVALUE ${convertOptions(lt.options)}"
        }
        case lt => {
          s"PARTITION ${lt.name} VALUES LESS THAN (${lt.value}) ${convertOptions(lt.options)}"
        }
      }
    }

    def converList(lt: ListRange) = {
      s"PARTITION ${lt.name} VALUES IN (${lt.value}) ${convertOptions(lt.options)}"
    }

    def convertOptions(ops: List[RangeOption]) = {
      ops.map(convertOption(_)).mkString(",")
    }

    def convertOption(op: RangeOption) = {
      op match{
        case RangeOption.Comment(c) => s"COMMENT = '${c}'"
        case RangeOption.Engine(e) => s"ENGINE = '${e}'"
        case RangeOption.DataDirectory(dir) => s"DATA DIRECTORY = '${dir}'"
        case RangeOption.IndexDirectory(dir) => s"INDEX DIRECTORY = '${dir}'"
        case RangeOption.MaxRows(size) => s"MAX_ROWS = ${size}"
        case RangeOption.MinRows(size) => s"MIN_ROWS = ${size}"
        case RangeOption.TableSpace(name) => s"TABLESPACE = ${name}"
        case RangeOption.NodeGroup(g) => s"NODEGROUP = ${g}"
      }
    }

    partition match {
      case KeyPartition(exp,size) => {
        s"PARTITION BY KEY(${exp}) PARTITIONS ${size}"
      }
      case HashPartition(exp,size) => {
        s"PARTITION BY HASH(${exp}) PARTITIONS ${size}"
      }
      case RangePartition(exp,ranges) => {
        s"PARTITION BY RANGE(${exp}) (${ranges.map(convertLessThan(_)).mkString(",\n")})"
      }
      case ColumnsPartition(cols,ranges) => {
        s"PARTITION BY RANGE COLUMNS(${cols.map(_.toString).mkString(",")}) (${ranges.map(convertLessThan(_)).mkString(",\n")})"
      }
      case ListPartition(exp,ranges) => {
        s"PARTITION BY LIST(${exp}) (${ranges.map(converList(_)).mkString(",\n")})"
      }
      case ListColumnsPartition(cols,ranges) => {
        s"PARTITION BY LIST COLUMNS(${cols.map(_.toString).mkString(",")}) (${ranges.map(converList(_)).mkString(",\n")})"
      }
      case _ => {
        ""
      }
    }
  }

  def toIndexDefinition(key : Key) = {
    val prefix = key match {
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
