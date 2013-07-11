package com.geishatokyo.diffsql

import PartialFunction._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

import scala.util.Try

import scala.collection.immutable.ListSet

trait SqlParser extends RegexParsers
    with DataTypes
    with TableOptions
    with ColumnOptions
    with Keys
{

  trait Definition

  case class Column(
    name: String,
    dataType: DataType
  )(val options: Set[ColumnOption]) extends Definition {
    override def toString =
      s"""$name $dataType """ + options.mkString(" ")
  }

  case class Table(
    name: String,
    columns: Set[Definition],
    options: Set[TableOption]
  ) {
    def alter(table: Table) = {
      val add = table.columns diff columns
      val drop = columns diff table.columns
      val opts = table.options diff options
      if (add.isEmpty && drop.isEmpty && opts.isEmpty)
        throw new RuntimeException("no difference")
      else
        s"ALTER TABLE $name " +
          (add.map("ADD " +).toList :::
            drop.map("DROP " +).toList :::
            opts.toList).mkString(",")
    }
  }

  case class CaseInsensitive(string: String) {
    def i = ("(?i)" + string).r
  }

  implicit def i(s: String) = CaseInsensitive(s)

  def Apply[A](p: Parser[A]) = """\(""".r ~> p <~ """\)""".r

  val value = """[\w`]+""".r

  val tableOption =
    TableOption.Engine | TableOption.Charset | TableOption.AutoIncrement

  val columnOption =
    NotNull |
    Null |
    PrimaryKey |
    UniqueKey |
    AutoIncrement

  val columnDefinition = value ~ dataType ~ rep(columnOption) ^^ {
    case name ~ typ ~ opts =>
      Column(name.toLowerCase, typ)(ListSet(opts:_*))
  }

  val createDinition =
    columnDefinition |
    Key.Primary |
    Key.Unique |
    Key.Index

  val dataType =
    Bit | TinyInt | SmallInt | MediumInt | Integer | Int | BigInt |
    Binary | VarBinary |
    Char | VarChar |
    Real | Double | Float |
    Decimal | Numeric |
    TinyText | Text | MediumText | LongText |
    DateTime | Date | TimeStamp | Time | Year |
    TinyBlob | Blob | MediumBlob | LongBlob

  def columns(defs: Set[Definition]) = defs.flatMap {
    case c: Column =>  c.options.collect {
      case PrimaryKey =>
        Key.Primary.Result(None, List(c.name)): Definition
      case UniqueKey =>
        Key.Unique.Result(None, List(c.name))
    } + c
    case x => Set(x)
  }

  val createTableStatement =
    "CREATE".i ~ "TABLE".i ~ opt("IF".i ~ "NOT".i ~ "EXISTS".i) ~>
    value ~ Apply(repsep(createDinition, ",".r)) ~
    rep(tableOption) <~ opt(";".r) ^^ {
      case name ~ defs ~ opts =>
        Table(name, columns(ListSet(defs: _*)), ListSet(opts: _*))
    }

  def parseSql(s: String) = parseAll(createTableStatement, s)

  def tryParse(s: String) = Try(parseSql(s).get)

  def diff(before: String, after: String) = for {
    x <- tryParse(before)
    y <- tryParse(after)
    z <- Try(x alter y)
  } yield z

}

object SqlParser extends SqlParser
