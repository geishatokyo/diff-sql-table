package com.geishatokyo.diff

import PartialFunction._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

import scala.collection.immutable.ListSet

sealed trait Definition
case class Column(name: String, dataType: DataType, options: Set[String]) extends Definition {
  override def toString = name + " " + dataType
  override def equals(a: Any) = cond(this -> a) {
    case (a: Column, b: Column) =>
      a.name == b.name && a.dataType == b.dataType
  }
}
case class Primary(names: List[String]) extends Definition
case class IndexKey(name: Option[String], columns: List[String]) extends Definition

object Definition {
  def columns(defs: List[Definition]) = {
    val cols = defs collect {
      case c: Column => c
    }
    val primary = defs.collect {
      case p: Primary => p.names
    }.flatten.toSet
    cols.map(c =>
      if (primary(c.name))
        c.copy(options = c.options + "PRIMARY KEY")
      else
        c
    )
  }
}

case class Table(name: String, columns: Set[Column], options: List[String]) {
  def alter(table: Table) = {
    s"ALTER TABLE $name " +
    ((table.columns diff columns).map("ADD " +).toList :::
    (columns diff table.columns).map("DROP " +).toList :::
    options).mkString(",")
  }
}

trait SqlParser extends RegexParsers with DataTypes {

  case class CaseInsensitive(string: String) {
    def re = ("(?i)" + string).r
  }

  implicit def i(s: String) = CaseInsensitive(s)

  case class Concat(parser: Parser[String]) {
    def ~~(value: Parser[String]) = parser ~ value ^^ {
      case a ~ b =>  s"$a $b"
    }
  }

  implicit def regex2concat(r: Regex) = Concat(r)

  implicit def parser2concat(p: Parser[String]) = Concat(p)

  def opts(parser: Parser[String]) = opt(parser) ^^ (_ getOrElse "")

  def appl[A](p: Parser[A]) = """\(""".r ~> p <~ """\)""".r

  def sum[A](list: List[Parser[A]]) = list.reduce(_ | _)

  val symbol = """[\w`]+""".r

  val string = """'[^']+'""".r

  val binary = "0".r | "1".r

  val tableOptions =
    List[(String, Parser[String])](
      "ENGINE" -> symbol,
      "AUTO_INCREMENT" -> symbol,
      "AVG_ROW_LENGTH" -> symbol,
      """(DEFAULT)\s+CHARACTER\s+SET""" -> symbol,
      """(DEFAULT)\s+CHARSET""" -> symbol,
      "CHECKSUM" -> binary,
      "COLLATE" -> symbol,
      "COMMENT" -> string,
      "CONNECTION" -> string,
      """DATA\s+DIRECTORY""" -> string,
      "DELAY_KEY_WRITE" -> binary,
      "INDEX DIRECTORY" -> string,
      "INSERT_METHOD" -> ("NO".re | "FIRST".re | "LAST".re),
      "KEY_BLOCK_SIZE" -> symbol,
      "MAX_ROWS" -> symbol,
      "MIN_ROWS" -> symbol,
      "PACK_KEYS" -> (binary | "DEFAULT".re),
      "PASSWORD" -> string,
      "ROW_FORMAT" -> ("DEFAULT".re | "DYNAMIC".re | "FIXED".re | "COMPRESSED".re | "REDUNDANT".re | "COMPACT".re),
      "UNION" -> (repsep(symbol, ",".r) ^^ (_.mkString(",")))
    )

  val tableOption =
    sum(tableOptions. map {
      case (key, value) => key.re ~ opt("=") ~ value ^^ {
        case key ~ equal ~ value => key + equal.getOrElse(" ") + value
      }
    })

  val columnOptions: List[Parser[String]] =
    List(
      """NOT\s+NULL""".re,
      "NULL".re,
      """DEFAULT""".re ~~ string,
      "AUTO_INCREMENT".re,
      "UNIQUE".re ~~ opts("KEY".re),
      opts("PRIMARY".re) ~~ "KEY".re
    )

  val columnDefinition =
    symbol ~ dataType ~ rep(sum(columnOptions)) ^^ {
      case name ~ typ ~ opts => Column(name.toLowerCase, typ, ListSet(opts:_*))
    }

  val createDinition =
    columnDefinition |
    """PRIMARY\s+KEY""".re ~ "(" ~> repsep(symbol, ",".r) <~ ")" ^^ Primary.apply | 
    ("KEY".re | "INDEX".re) ~> (opt(symbol) <~ "(") ~ repsep(symbol, ",".r) <~ ")" ^^ {
      case name ~ cols => IndexKey(name, cols)
    }

  val createTable = "CREATE".re ~ "TABLE".re ~ opt("""IF\s+NOT\s+EXISTS""".re) ~> symbol

  val createTableStatement = createTable ~ appl(repsep(createDinition, ",".r)) ~ rep(tableOption) <~ opt(";".r) ^^ {
    case name ~ defs ~ opts =>
      Table(name, ListSet(Definition.columns(defs): _*), opts)
  }

  def parseSql(s: String) = parseAll(createTableStatement, s)

  def diff(a: String, b: String): String =
    (parseSql(a), parseSql(b)) match {
      case (Success(a, _), Success(b, _)) => a alter b
      case (a: NoSuccess, b: NoSuccess) => a + "\n" + b
      case (a: NoSuccess, _) => a.toString
      case (_, b: NoSuccess) => b.toString
    }

  def diffOp(a: String, b: String) = diff(b, a)

}

object SqlParser extends SqlParser
