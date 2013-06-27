package com.geishatokyo.diff

trait TableOptions { self: SqlParser =>
  import TableOptions._
  sealed abstract class TableOption(parser: Parser[Any], key: String)
      extends Parser[Result] {
    def this(key: String) = this(key.i, key)
    def apply(in: Input) =
      parse(parser ~ opt("=") ~> value ^^ (Result(key, _)), in)
  }
  object TableOption {
    case object Engine extends TableOption("ENGINE")
    case object Charset extends TableOption(
      opt("DEFAULT".i) ~ ("CHARACTER".i ~ "SET".i | "CHARSET".i),
      "CHARACTER SET"
    )
    case object AutoIncrement extends TableOption("AUTO_INCREMENT")
  }
}

object TableOptions extends Options {
  case class Result(key: String, value: String)
      extends Value(s"$key=$value")
}
