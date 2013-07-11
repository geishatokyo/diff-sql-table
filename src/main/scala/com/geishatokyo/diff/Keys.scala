package com.geishatokyo.diff

trait Keys { self: SqlParser =>

  sealed abstract class Key(key: String, parser: Parser[Any])
      extends Parser[Definition] {
    case class Result(
      name: Option[String],
      columns: List[String]
    ) extends Definition {
      override def toString =
        s"""$key ${name getOrElse ""} ${columns mkString " "}"""
    }
    def apply(in: Input) =
      parse(parser ~> opt(value) ~ Apply(repsep(value, ",".r)) ^^ {
        case name ~ columns =>
          Result(name.map(_.toLowerCase), columns.map(_.toLowerCase))
      }, in)
  }

  object Key {
    case object Primary extends Key("PRIMARY KEY","PRIMARY".i ~ "KEY".i)
    case object Unique extends Key("UNIQUE KEY", "UNIQUE".i ~ "KEY".i)
    case object Index extends Key("KEY", "KEY".i | "INDEX".i)
  }

}
