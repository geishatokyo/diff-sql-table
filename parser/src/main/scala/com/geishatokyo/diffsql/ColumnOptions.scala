package com.geishatokyo.diffsql

sealed trait ColumnOption

trait ColumnOptions { self: SqlParser =>

  object ColumnOption {

    sealed abstract class Parser(key: String, parser: self.Parser[Any])
        extends self.Parser[ColumnOption] {
      case object Value extends ColumnOption {
        override def toString = key
      }
      def this(key: String) = this(key, key.i)
      def apply(in: Input) = parse(parser ^^^ Value, in)
    }

    case object Null extends Parser("NULL")
    case object AutoIncrement extends Parser("AUTO_INCREMENT")
    case object NotNull extends Parser("NOT NULL", "NOT".i ~ "NULL".i)
    case object PrimaryKey extends Parser(
      "PRIMARY KEY",
      opt("PRIMARY".i) ~ "KEY".i
    )
    case object UniqueKey extends Parser(
      "UNIQUE KEY",
      "UNIQUE".i ~ opt("KEY".i)
    )
    case object Default extends self.Parser[ColumnOption] {
      case class Value(value: String) extends ColumnOption
      def apply(in: Input) =
        parse("DEFAULT".i ~> value ^^ Value.apply, in)
    }
  }

}
