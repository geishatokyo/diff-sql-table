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
  options: Set[TableOption]
) {
  def alter(table: Table) = {
    val add = table.columns diff columns map ("ADD " +)
    val drop = columns diff table.columns map ("DROP " +)
    val opts = table.options diff options
    if (add.isEmpty && drop.isEmpty && opts.isEmpty)
      None
    else
      Some(s"ALTER TABLE $name " + (add ++ drop ++ opts).mkString(","))
  }
}

trait SqlParser extends RegexParsers
    with DataTypes
    with TableOptions
    with ColumnOptions
    with Keys
{

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

  def diff(before: String, after: String) = for {
    before <- Try(parseSql(before).get)
    after <- Try(parseSql(after).get)
    sql <- Try(before alter after get)
  } yield sql

}

object SqlParser extends SqlParser
