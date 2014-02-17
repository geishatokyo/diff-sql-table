package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.ast.DataType

/**
 * Created by takeshita on 14/02/14.
 */
trait DataTypeParsers { self : SQLParser =>

  object DataTypeDef{

    val Length = "(" ~> """\d+""".r  <~ ")" ^^ (_.toInt)

    class DataTypeWithLength(name : String) extends self.Parser[DataType]{
      def apply( in : Input) = parse(name ~> opt(Length) ^^ {
        case length => DataType(name,length.toList)
      },in)
    }
    class DataTypeWithPrecision(name : String) extends self.Parser[DataType]{

      def precisionPhrase : Parser[List[Int]] = ("PRECISION" ~ "(") ~> digits ~ "," ~ digits <~ ")" ^^ {
        case d1 ~ "," ~ d2 => {
          List(d1,d2)
        }
      }
      def precisionArg : Parser[List[Int]] = "(" ~> digits ~ "," ~ digits <~ ")" ^^ {
        case d1 ~ "," ~ d2 => {
          List(d1,d2)
        }
      }

      def apply( in : Input) = parse(name ~> opt( precisionPhrase | precisionArg ) ^^ {
        case precision => DataType(name,precision.getOrElse(Nil))
      },in)
    }
    class DataTypeOnly(name : String) extends self.Parser[DataType]{
      def apply(in: Input) = parse( name  ^^^ {
        DataType(name,Nil)
      },in)
    }


    case object Bit extends DataTypeWithLength("BIT")
    case object Bool extends DataTypeWithLength("BOOL")
    case object Boolean extends DataTypeWithLength("BOOLEAN")
    case object TinyInt extends DataTypeWithLength("TINYINT")
    case object SmallInt extends DataTypeWithLength("SMALLINT")
    case object MediumInt extends DataTypeWithLength("MEDIUMINT")
    case object Integer extends DataTypeWithLength("INTEGER")
    case object Int extends DataTypeWithLength("INT")
    case object BigInt extends DataTypeWithLength("BIGINT")

    case object Binary extends DataTypeWithLength("BINARY")
    case object VarBinary extends DataTypeWithLength("VARBINARY")


    case object Char extends DataTypeWithLength("CHAR")
    case object VarChar extends DataTypeWithLength("VARCHAR")

    case object Real extends DataTypeWithPrecision("REAL")
    case object Double extends DataTypeWithPrecision("DOUBLE")
    case object Float extends DataTypeWithPrecision("FLOAT")

    case object Decimal extends DataTypeWithPrecision("DECIMAL")
    case object Numeric extends DataTypeWithPrecision("NUMERIC")


    case object TinyText extends DataTypeOnly("TINYTEXT")
    case object Text extends DataTypeOnly("TEXT")
    case object MediumText extends DataTypeOnly("MEDIUMTEXT")
    case object LongText extends DataTypeOnly("LONGTEXT")


    case object DateTime extends DataTypeOnly("DATETIME")
    case object Date extends DataTypeOnly("DATE")
    case object TimeStamp extends DataTypeOnly("TIMESTAMP")
    case object Time extends DataTypeOnly("TIME")
    case object Year extends DataTypeOnly("YEAR")
    case object TinyBlob extends DataTypeOnly("TINYBLOB")
    case object Blob extends DataTypeOnly("BLOB")
    case object MediumBlob extends DataTypeOnly("MEDIUMBLOB")
    case object LongBlob extends DataTypeOnly("LONGBLOB")
  }

  val dataType = { import DataTypeDef._
    Bit | Boolean | Bool | TinyInt | SmallInt | MediumInt | Integer | Int | BigInt |
      Binary | VarBinary |
      Char | VarChar |
      Real | Double | Float |
      Decimal | Numeric |
      TinyText | Text | MediumText | LongText |
      DateTime | Date | TimeStamp | Time | Year |
      TinyBlob | Blob | MediumBlob | LongBlob
  }




}
