package com.geishatokyo.diffsql

import scala.util.parsing.combinator.RegexParsers
import scala.util.Try

trait Definition

case class Column
  (name: String, dataType: DataType)
  (val options: Set[ColumnOption])
    extends Definition {
  override def toString =
    s"""$name $dataType """ + options.mkString(" ")
}

case class Table(
  name: String,
  columns: Set[Definition],
  options: Set[TableOption])

case class Diff(
  name: String,
  add: Set[Definition],
  drop: Set[String],
  modify: Set[Column],
  options: Set[TableOption]) {
  override def toString = {
    val ADD = add.map("ADD " +)
    val DROP = drop.map("DROP " +)
    val MODIFY = modify.map("MODIFY " +)
    s"ALTER TABLE $name " +
      (ADD ++ DROP ++ MODIFY ++ options).mkString(",")
  }
}

trait SqlParser extends RegexParsers
    with DataTypes
    with TableOptions
    with ColumnOptions
    with Keys {

  implicit class CaseInsensitive(string: String) {
    def i = ("(?i)" + string).r
  }

  def Apply[A](p: Parser[A]) = """\(""".r ~> p <~ """\)""".r

  val value = """[\w`]+""".r

  val tableOption = { import TableOption._
    Engine | Charset | AutoIncrement
  }

  val columnOption = { import ColumnOption._
    NotNull | Null | PrimaryKey | UniqueKey | AutoIncrement
  }

  val columnDefinition = value ~ dataType ~ rep(columnOption) ^^ {
    case name ~ typ ~ opts =>
      Column(name.toLowerCase, typ)(Set(opts:_*))
  }

  val createDinition = { import Key._
    columnDefinition | Primary | Unique | Index
  }

  val dataType = { import DataType._
    Bit | TinyInt | SmallInt | MediumInt | Integer | Int | BigInt |
    Binary | VarBinary |
    Char | VarChar |
    Real | Double | Float |
    Decimal | Numeric |
    TinyText | Text | MediumText | LongText |
    DateTime | Date | TimeStamp | Time | Year |
    TinyBlob | Blob | MediumBlob | LongBlob
  }

  def expand(defs: Set[Definition]) = defs.flatMap {
    case c: Column => import ColumnOption._, Key._
      c.options.collect {
        case PrimaryKey.Value =>
          Primary.Value(None, List(c.name)): Definition
        case UniqueKey.Value =>
          Unique.Value(None, List(c.name)): Definition
      } + c
    case x => Set(x)
  }

  val createTableStatement =
    "CREATE".i ~ "TABLE".i ~ opt("IF".i ~ "NOT".i ~ "EXISTS".i) ~>
    value ~ Apply(repsep(createDinition, ",".r)) ~
    rep(tableOption) <~ opt(";".r) ^^ {
      case name ~ defs ~ opts =>
        Table(name, expand(Set(defs: _*)), Set(opts: _*))
    }

  def parseSql(s: String) = parseAll(createTableStatement, s)

  def diff(before: Table, after: Table): Diff

  def diff(before: String, after: String): Try[Diff] = for {
    before <- Try(parseSql(before).get)
    after <- Try(parseSql(after).get)
    sql <- Try(diff(before, after))
  } yield sql

}

object SqlParser extends SqlParser with Differ with LaxEqualizer

trait Differ { self: SqlParser =>
  def diff(before: Table, after: Table) = {
    val changes =
    before.columns.collect {
      case b: Column => after.columns.collect {
        case a: Column if a.name == b.name && a.dataType != b.dataType => b
      }
    }.flatten
    def remove(defs: Set[Definition]) =
    defs filter {
      case c: Column => !changes.map(_.name).contains(c.name)
      case _ => true
    }
    Diff(before.name,
      remove(before.columns diff after.columns),
      remove(after.columns diff before.columns).collect {
        case c: Column => c.name
      },
      changes,
      before.options diff after.options)
  }
}

trait LaxEqualizer { self: SqlParser =>
  def equal(x: DataType, y: DataType) = x.hashCode == y.hashCode
}

trait StrictEqualizer { self: SqlParser =>
  def equal(x: DataType, y: DataType) =
    x.hashCode == y.hashCode && x.fields == y.fields
}
