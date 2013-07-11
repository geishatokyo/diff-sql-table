package com.geishatokyo.diffsql

import PartialFunction._

import scala.collection.immutable.ListSet

sealed trait DataType

trait DataTypes { self: SqlParser =>

  trait TypeParser extends Parser[DataType] {
    val length = """\d+""".r ^^ (_.toInt)
    val charset =
      "CHARACTER".i ~ "SET".i ~> value ^^ (name => s"CHARACTER SET $name")
    def parser: Parser[DataType]
    def apply(input: Input) =
      parse(parser, input)
  }

  abstract class Integral(name: String) extends TypeParser {
    case class Result()(length: Option[Int]) extends DataType {
      override def toString =
        s"$name" + length.map("(" + _ + ")").getOrElse("")
    }
    def parser = name.i ~> opt(Apply(length)) ^^ Result()
  }
  case object Bit extends Integral("BIT")
  case object TinyInt extends Integral("TINYINT")
  case object SmallInt extends Integral("SMALLINT")
  case object MediumInt extends Integral("MEDIUMINT")
  case object Integer extends Integral("INTEGER")
  case object Int extends Integral("INT")
  case object BigInt extends Integral("BIGINT")


  abstract class Binary(name: String) extends TypeParser { self =>
    case class Result()(length: Int) extends DataType {
      override def toString = s"$name($length)"
    }
    def parser = name.i ~> Apply(length) ^^ Result()
  }
  case object Binary extends Binary("BINARY")
  case object VarBinary extends Binary("VARBINARY")

  abstract class Character(name: String) extends TypeParser { self =>
    case class Result()(length: Int, charset: Option[String]) extends DataType {
      override def toString = s"$name($length)" + charset.getOrElse("")
    }
    def parser =
      name.i ~> Apply(length) ~ opt(charset) ^^ {
        case length ~ charset => Result()(length, charset)
      }
  }
  case object Char extends Character("CHAR")
  case object VarChar extends Character("VARCHAR")

  abstract class Floating(name: String) extends TypeParser {
    abstract class Pair(length: Option[Int ~ Int]) extends DataType {
      override def toString = length match {
        case Some(length ~ decimals) => s"$name($length, $decimals)"
        case _ => name
      }
    }
    def result: Option[Int ~ Int] => Pair
    def parser =
      name.i ~> opt(Apply((length <~ ",") ~ length)) ^^ result
  }

  abstract class Real(name: String) extends Floating(name) {
    case class Result()(length: Option[Int ~ Int])
        extends Pair(length)
    val result = Result()_
  }
  case object Real extends Real("REAL")
  case object Double extends Real("DOUBLE")
  case object Float extends Real("FLOAT")

  abstract class Numeric(name: String) extends Floating(name) {
    case class Result()(length: Option[Int ~ Int])
        extends Pair(length)
    val result = Result()_
  }
  case object Decimal extends Numeric("DECIMAL")
  case object Numeric extends Numeric("NUMERIC")

  abstract class Text(name: String) extends TypeParser {
    case class Result()(charset: Option[String]) extends DataType {
      override def toString = name + charset.getOrElse("")
    }
    def parser = name.i ~> opt(charset) ^^ Result()
  }
  case object TinyText extends Text("TINYTEXT")
  case object Text extends Text("TEXT")
  case object MediumText extends Text("MEDIUMTEXT")
  case object LongText extends Text("LONGTEXT")

  abstract class Simple(name: String) extends TypeParser { self => 
    case class Result(name: String) extends DataType {
      override def toString = self.name
    }
    def parser = name.i ^^ Result.apply
  }
  case object DateTime extends Simple("DATETIME")
  case object Date extends Simple("DATE")
  case object TimeStamp extends Simple("TIMESTAMP")
  case object Time extends Simple("TIME")
  case object Year extends Simple("YEAR")
  case object TinyBlob extends Simple("TINYBLOB")
  case object Blob extends Simple("BLOB")
  case object MediumBlob extends Simple("MEDIUMBLOB")
  case object LongBlob extends Simple("LONGBLOB")

}
