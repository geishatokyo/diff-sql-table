package com.geishatokyo.diff

trait ColumnOptions { self: SqlParser =>
  import ColumnOptions._
  sealed abstract class ColumnOption(parser: Parser[Any], key: String)
      extends Parser[Result] {
    def this(key: String) = this(key.i, key)
    def apply(in: Input) = parse(parser ^^^ Result(key), in)
  }
  object ColumnOption {
    case object Null extends ColumnOption("NULL")
    case object AutoIncrement extends ColumnOption("AUTO_INCREMENT")
    case object NotNull extends ColumnOption(
      "NOT".i ~ "NULL".i,
      "NOT NULL"
    )
    case object PrimaryKey extends ColumnOption(
      opt("PRIMARY".i) ~ "KEY".i,
      "PRIMARY KEY"
    )
    case object UniqueKey extends ColumnOption(
      "UNIQUE".i ~ opt("KEY".i),
      "UNIQUE KEY"
    )
  }
}

object ColumnOptions extends Options {
  case class Result(key: String) extends Value(key)
}
