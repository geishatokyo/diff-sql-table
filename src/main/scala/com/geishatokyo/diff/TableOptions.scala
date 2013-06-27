package com.geishatokyo.diff

trait TableOptions { self: SqlParser =>
  import TableOptions._
  sealed abstract class TableOption(key: String, parser: Parser[Any])
      extends Parser[Result] {
    def this(key: String) = this(key, key.i)
    def apply(in: Input) =
      parse(parser ~ opt("=") ~> value ^^ (Result(key, _)), in)
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

object TableOptions {
  case class Result(key: String, value: String) {
    override def toString = s"$key=$value"
  }
}
