package com.geishatokyo.diffsql

sealed trait TableOption

trait TableOptions { self: SqlParser =>

  object TableOption {

    sealed abstract class Parser(key: String, parser: self.Parser[Any])
        extends self.Parser[TableOption] {
      case class Value(value: String) extends TableOption {
        override def toString = s"$key=$value"
      }
      def this(key: String) = this(key, key.i)
      def apply(in: Input) =
        parse(parser ~ opt("=") ~> value ^^ Value.apply, in)
    }

    case object Engine extends Parser("ENGINE")
    case object Charset extends Parser(
      "CHARACTER SET",
      opt("DEFAULT".i) ~ ("CHARACTER".i ~ "SET".i | "CHARSET".i)
    )
    case object AutoIncrement extends Parser("AUTO_INCREMENT")

  }

}
