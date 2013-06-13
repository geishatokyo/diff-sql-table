package com.geishatokyo.diff

import PartialFunction._

import scala.collection.immutable.ListSet

sealed trait DataType {
  import DataTypes._
  val name: String
  val options: Set[String]
  def ops = options.mkString(" ")
  override def equals(a: Any) = cond(a) {
    case d: DataType =>
      name == d.name || types.exists(t => t(name) && t(d.name))
  }
}

case class Integral(
  name: String,
  length: Option[Int],
  options: Set[String]
) extends DataType {
  override def toString = {
    val len = length.map(n => "(" + n + ")").getOrElse("")
    s"$name $len $ops"
  }
}

case class Floating(
  name: String,
  length: Option[(Int, Int)],
  options: Set[String]
) extends DataType {
  override def toString = {
    val len = length.map(_.toString).getOrElse("")
    s"$name $len $ops"
  }
}

case class Simple(name: String, options: Set[String]) extends DataType {
  override def toString =
    s"$name $ops"
}

object DataTypes {

  val int =
    List(
      "BIT",
      "TINYINT",
      "SMALLINT",
      "MEDIUMINT",
      "INTEGER",
      "INT",
      "BIGINT"
    )

  val char = List("CHAR", "VARCHAR")

  val binary = List("BINARY", "VARBINARY")

  val integrals = int ::: char ::: binary

  val floatings = List("REAL", "DOUBLE", "FLOAT", "DECIMAL", "NUMERIC")

  val text =
    List(
      "TINYTEXT",
      "TEXT",
      "MEDIUMTEXT",
      "LONGTEXT"
    )

  val simples =
    List(
      "DATETIME",
      "DATE",
      "TIMESTAMP",
      "TIME",
      "YEAR",
      "TINYBLOB",
      "BLOB",
      "MEDIUMBLOB",
      "LONGBLOB"
    ) ::: text

  val options = List(
    "UNSIGNED",
    "ZEROFILL",
    """CHARACTER\s+SET\s+\S+""",
    """COLLATE\s+\S+""",
    "BINARY"
  )

  val types = List(int, char ::: text, floatings).map(a => ListSet(a: _*))

}

trait DataTypes { self: SqlParser =>
  import DataTypes._

  val option =
    rep(sum(options.map(_.re: Parser[String]))) ^^ (a => ListSet(a: _*))

  def integral(name: String) =
    name.re ~ opt(appl("""\d+""".r)) ~ option ^^ {
      case name ~ length ~ options =>
        Integral(name.toUpperCase, length.map(_.toInt), options)
    }

  def floating(name: String) =
    name.re ~ opt(appl(("""\d+""".r <~ ",".r) ~ """\d+""".r)) ~ option ^^ {
      case name ~ pair ~ options =>
        Floating(name.toUpperCase, pair map {
          case length ~ decimals => length.toInt -> decimals.toInt
        }, options)
    }

  def simple(name: String) =
    name.re ~ option ^^ {
      case name ~ options => Simple(name.toUpperCase, options)
    }

  val dataType =
    sum(
      integrals.map(integral) :::
      floatings.map(floating) :::
      simples.map(simple)
    )

}
