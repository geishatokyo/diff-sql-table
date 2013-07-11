package com.geishatokyo.diffsql

trait ColumnOptions { self: SqlParser =>

  sealed abstract class ColumnOption(
    key: String,
    parser: Parser[Any]
  ) extends Parser[ColumnOption] {
    def this(key: String) = this(key, key.i)
    def apply(in: Input) = parse(parser ^^^ this, in)
    override def toString = key
  }

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
