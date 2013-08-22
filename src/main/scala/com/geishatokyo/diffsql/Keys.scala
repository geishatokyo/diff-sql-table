package com.geishatokyo.diffsql

sealed trait Key extends Definition

trait Keys { self: SqlParser =>

  object Key {

    abstract class Parser(key: String, parser: self.Parser[Any])
        extends self.Parser[Key] {
      case class Value(
        key: String,
        name: Option[String],
        columns: List[String])
          extends Key {
        override def toString =
          s"""$key ${name getOrElse ""} ${columns mkString " "}"""
      }
      object Value {
        def apply(
          name: Option[String] = None,
          columns: List[String] = Nil)
            : Value = Value(key, name, columns)
      }
      def apply(in: Input) =
        parse(parser ~> opt(value) ~ Apply(repsep(value, ",".r)) ^^ {
          case name ~ columns =>
            Value(name.map(_.toLowerCase), columns.map(_.toLowerCase))
        }, in)
    }

    case object Primary extends Parser("PRIMARY KEY","PRIMARY".i ~ "KEY".i)
    case object Unique extends Parser("UNIQUE KEY", "UNIQUE".i ~ "KEY".i)
    case object Index extends Parser("KEY", "KEY".i | "INDEX".i)

  }

}
