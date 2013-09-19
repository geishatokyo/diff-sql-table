package com.geishatokyo.diffsql

import scala.util.parsing.combinator.RegexParsers

trait Definition

case class Column
  (name: String, dataType: DataType)
  (val options: Set[ColumnOption])
    extends Definition {
  override def toString =
    name + " " + dataType + " " + options.mkString(" ")
}

sealed trait Result {
  val name: String
}

case class Table(
  name: String,
  columns: Set[Definition],
  options: Set[TableOption]) { self =>
  def create = new Result {
    val name = self.name
    override def toString =
      "CREATE TABLE " + name + " ( " +
      columns.mkString(",") +
      " );"
  }
  def drop = new Result {
    val name = self.name
    override def toString =
      "DROP TABLE " + name
  }
}

case class Diff(
  name: String,
  add: Set[Definition],
  drop: Set[String],
  modify: Set[Column],
  options: Set[TableOption])
    extends Result {
  override def toString = {
    val ADD = add.map("ADD " +)
    val DROP = drop.map("DROP " +)
    val MODIFY = modify.map("MODIFY " +)
    "ALTER TABLE " + name + " " +
      (ADD ++ DROP ++ MODIFY ++ options).mkString(",")
  }
}

trait SqlParser extends RegexParsers
    with DataTypes
    with TableOptions
    with ColumnOptions
    with Keys {

  class CaseInsensitive(string: String) {
    def i = ("(?i)" + string).r
  }
  implicit def caseInsensitive(string: String) = new CaseInsensitive(string)

  def Apply[A](p: Parser[A]) = """\(""".r ~> p <~ """\)""".r

  val value = """[\w`]+""".r

  val tableOption = { import TableOption._
    Engine | Charset | AutoIncrement
  }

  val columnOption = { import ColumnOption._
    NotNull | Null | PrimaryKey | UniqueKey | AutoIncrement | Default
  }

  val columnDefinition = value ~ dataType ~ rep(columnOption) ^^ {
    case name ~ typ ~ opts =>
      Column(name.toLowerCase, typ)(Set(opts:_*))
  }

  val createDinition = { import Key._
    columnDefinition | Primary | Unique | Index
  }

  val dataType = { import DataType._
    Bit | Boolean | Bool | TinyInt | SmallInt | MediumInt | Integer | Int | BigInt |
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
        Table(name.toUpperCase, expand(Set(defs: _*)), Set(opts: _*))
    }

  def parseSql(s: String) = parseAll(rep(createTableStatement), s) match {
    case Success(result, _) => result
    case nosuccess: NoSuccess =>
      throw new RuntimeException(nosuccess.toString)
  }

  def diff(before: Table, after: Table): Option[Diff]

  def diff(after: String, before: String): Option[Diff] =
    diff(parseSql(before).head, parseSql(after).head)

  def genSql(a: String, b: String): Set[Result] = {
    val before = parseSql(b)
    val after = parseSql(a)
    (before.map(_.name) ++ after.map(_.name)).toSet.flatMap { (name: String) =>
      val b = before.groupBy(_.name).mapValues(_.head)
      val a = after.groupBy(_.name).mapValues(_.head)
      ((b get name, a get name): @unchecked) match {
        case (Some(before), Some(after)) =>
          diff(before, after)
        case (None, Some(after)) => Some(after.create)
        case (Some(before), None) => Some(before.drop)
      }
    }
  }

}

object SqlParser extends SqlParser with Differ with LaxEqualizer

trait Differ { self: SqlParser =>
  def diff(before: Table, after: Table): Option[Diff] = {
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
    val add = remove(before.columns diff after.columns)
    val drop = remove(after.columns diff before.columns).collect {
      case c: Column => c.name
    }
    val ops = before.options diff after.options
    if (changes.isEmpty && add.isEmpty && drop.isEmpty && ops.isEmpty)
      None
    else
      Some(Diff(before.name, add, drop, changes, ops))
  }
}

trait LaxEqualizer { self: SqlParser =>
  def equal(x: DataType, y: DataType) = x.hashCode == y.hashCode
}

trait StrictEqualizer { self: SqlParser =>
  def equal(x: DataType, y: DataType) =
    x.hashCode == y.hashCode && x.fields == y.fields
}
