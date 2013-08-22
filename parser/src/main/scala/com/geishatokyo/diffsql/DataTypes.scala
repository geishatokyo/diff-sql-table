package com.geishatokyo.diffsql

sealed trait DataType {
  val fields: Any
}

trait DataTypes { self: SqlParser =>

  def equal(x: DataType, y: DataType): Boolean
  trait Equalizer { self: DataType =>
    override def equals(x: Any) = x match {
      case t: DataType => equal(t, this)
      case _ => false
    }
  }

  object DataType {

    trait Parser extends self.Parser[DataType] {
      val Length = """\d+""".r ^^ (_.toInt)
      val Charset =
        "CHARACTER".i ~ "SET".i ~> value ^^ (name => s"CHARACTER SET $name")
      val parser: self.Parser[DataType]
      def apply(input: Input) = parse(parser, input)
    }

    abstract class Integral(name: String) extends Parser {
      val parser = name.i ~> opt(Apply(Length)) ^^ (length =>
        new DataType with Equalizer {
          val fields = length
          override def hashCode = Int##
          override def toString =
            name + length.map("(" + _ + ")").getOrElse("")
        })
    }

    case object Bit extends Integral("BIT")
    case object TinyInt extends Integral("TINYINT")
    case object SmallInt extends Integral("SMALLINT")
    case object MediumInt extends Integral("MEDIUMINT")
    case object Integer extends Integral("INTEGER")
    case object Int extends Integral("INT")
    case object BigInt extends Integral("BIGINT")

    abstract class Binary(name: String) extends Parser {
      val parser = name.i ~> Apply(Length) ^^ (length =>
        new DataType with Equalizer {
          val fields = length
          override def hashCode = Binary##
          override def toString = s"$name($length)"
        })
    }

    case object Binary extends Binary("BINARY")
    case object VarBinary extends Binary("VARBINARY")

    abstract class Character(name: String) extends Parser {
      val parser = name.i ~> Apply(Length) ~ opt(Charset) ^^ {
        case length ~ charset => new DataType with Equalizer {
          val fields = (length, charset)
          override def hashCode = Char##
          override def toString = s"$name($length)" + charset.getOrElse("")
        }
      }
    }

    case object Char extends Character("CHAR")
    case object VarChar extends Character("VARCHAR")

    abstract class Real(name: String) extends Parser {
      val parser = name.i ~> opt(Apply((Length <~ ",") ~ Length)) ^^ (length =>
        new DataType with Equalizer {
          val fields = length
          override def hashCode = Real##
          override def toString = length match {
            case Some(a ~ b) => s"$name($a, $b)"
            case None => name
          }
        })
    }

    case object Real extends Real("REAL")
    case object Double extends Real("DOUBLE")
    case object Float extends Real("FLOAT")

    case object Decimal extends Real("DECIMAL")
    case object Numeric extends Real("NUMERIC")

    abstract class Text(name: String) extends Parser {
      val parser = name.i ~> opt(Charset) ^^ (charset =>
        new DataType with Equalizer {
          val fields = charset
          override def hashCode = Text##
          override def toString = name + charset.getOrElse("")
        }
      )
    }

    case object TinyText extends Text("TINYTEXT")
    case object Text extends Text("TEXT")
    case object MediumText extends Text("MEDIUMTEXT")
    case object LongText extends Text("LONGTEXT")

    abstract class Simple(name: String) extends Parser { self =>
      val parser = name.i ^^ (name => new DataType {
        val fields = ()
        override def toString = self.name
      })
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
