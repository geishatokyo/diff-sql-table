package com.geishatokyo.diff

trait TableOptions { self: SqlParser =>
  sealed abstract class TableOption(key: String, parser: Parser[Any])
      extends Parser[TableOption] {
    case class Result(value: String) extends TableOption(key, parser) {
      override def toString = s"$key=$value"
    }
    def this(key: String) = this(key, key.i)
    def apply(in: Input) =
      parse(parser ~ opt("=") ~> value ^^ Result.apply, in)
  }
  object TableOption {
    case object Engine extends TableOption("ENGINE")
    case object Charset extends TableOption(
      "CHARACTER SET",
      opt("DEFAULT".i) ~ ("CHARACTER".i ~ "SET".i | "CHARSET".i)
    )
    case object AutoIncrement extends TableOption("AUTO_INCREMENT")
  }
}
