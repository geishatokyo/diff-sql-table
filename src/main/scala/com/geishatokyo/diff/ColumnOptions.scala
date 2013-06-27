package com.geishatokyo.diff

trait ColumnOptions { self: SqlParser =>
  import ColumnOptions._
  sealed abstract class ColumnOption(key: String, parser: Parser[Any])
      extends Parser[Result] {
    def this(key: String) = this(key, key.i)
    def apply(in: Input) = parse(parser ^^^ Result(key), in)
  }
  object ColumnOption {
    case object Null extends ColumnOption("NULL")
    case object AutoIncrement extends ColumnOption("AUTO_INCREMENT")
    case object NotNull extends ColumnOption(
      "NOT NULL",
      "NOT".i ~ "NULL".i
    )
    case object PrimaryKey extends ColumnOption(
      "PRIMARY KEY",
      opt("PRIMARY".i) ~ "KEY".i
    )
    case object UniqueKey extends ColumnOption(
      "UNIQUE KEY",
      "UNIQUE".i ~ opt("KEY".i)
    )
  }
}

object ColumnOptions {
  case class Result(key: String) {
    override def toString = key
  }
}
