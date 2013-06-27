package com.geishatokyo.diff

trait Keys { self: SqlParser =>
  import Keys._
  sealed abstract class Key(key: String, parser: Parser[Any])
      extends Parser[Result] {
    def apply(in: Input) =
      parse(parser ~> (opt(value) <~ "(") ~ repsep(value, ",".r) <~ ")" ^^ {
        case name ~ columns => Result(key, name, columns)
      }, in)
  }
  object Key {
    case object Primary extends Key(
      "PRIMARY KEY",
      "PRIMARY".i ~ "KEY".i
    )
    case object Unique extends Key("UNIQUE KEY", "UNIQUE".i ~ "KEY".i)
    case object Index extends Key("KEY", "KEY".i | "INDEX".i)
  }
}

object Keys {
  case class Result(
    key: String,
    name: Option[String],
    columns: List[String]
  ) extends Definition {
    override def toString =
      s"""$key ${name getOrElse ""} ${columns mkString " "}"""
  }
}
