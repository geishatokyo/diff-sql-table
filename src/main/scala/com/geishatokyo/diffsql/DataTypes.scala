package com.geishatokyo.diffsql

sealed trait DataType

trait DataTypes { self: SqlParser =>

  object DataType {

    trait Parser extends self.Parser[DataType] {
      val Length = """\d+""".r ^^ (_.toInt)
      val Charset =
        "CHARACTER".i ~ "SET".i ~> value ^^ (name => s"CHARACTER SET $name")
      val parser: self.Parser[DataType]
      def apply(input: Input) = parse(parser, input)
    }

    abstract class Integral(name: String) extends Parser {
      case class Result()(length: Option[Int]) extends DataType {
        override def toString =
          name + length.map("(" + _ + ")").getOrElse("")
      }
      val parser = name.i ~> opt(Apply(Length)) ^^ Result()
    }

    case object Bit extends Integral("BIT")
    case object TinyInt extends Integral("TINYINT")
    case object SmallInt extends Integral("SMALLINT")
    case object MediumInt extends Integral("MEDIUMINT")
    case object Integer extends Integral("INTEGER")
    case object Int extends Integral("INT")
    case object BigInt extends Integral("BIGINT")

    abstract class Binary(name: String) extends Parser {
      case class Result()(length: Int) extends DataType {
        override def toString = s"$name($length)"
      }
      val parser = name.i ~> Apply(Length) ^^ Result()
    }

    case object Binary extends Binary("BINARY")
    case object VarBinary extends Binary("VARBINARY")

    abstract class Character(name: String) extends Parser {
      case class Result()(length: Int, charset: Option[String])
          extends DataType {
        override def toString = s"$name($length)" + charset.getOrElse("")
      }
      val parser = name.i ~> Apply(Length) ~ opt(Charset) ^^ {
        case length ~ charset => Result()(length, charset)
      }
    }

    case object Char extends Character("CHAR")
    case object VarChar extends Character("VARCHAR")

    abstract class Real(name: String) extends Parser {
      case class Result(length: Option[(Int, Int)]) extends DataType {
        override def toString = name + length.map(_.toString).getOrElse("")
      }
      val parser = name.i ~> opt(Apply((Length <~ ",") ~ Length)) ^^ {
        case Some(a ~ b) => Result(Some(a -> b))
        case None => Result(None)
      }
    }

    case object Real extends Real("REAL")
    case object Double extends Real("DOUBLE")
    case object Float extends Real("FLOAT")

    case object Decimal extends Real("DECIMAL")
    case object Numeric extends Real("NUMERIC")

    abstract class Text(name: String) extends Parser {
      case class Result()(charset: Option[String]) extends DataType {
        override def toString = name + charset.getOrElse("")
      }
      val parser = name.i ~> opt(Charset) ^^ Result()
    }

    case object TinyText extends Text("TINYTEXT")
    case object Text extends Text("TEXT")
    case object MediumText extends Text("MEDIUMTEXT")
    case object LongText extends Text("LONGTEXT")

    abstract class Simple(name: String) extends Parser { self =>
      case class Result(name: String) extends DataType {
        override def toString = self.name
      }
      val parser = name.i ^^ Result.apply
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

}