package com.geishatokyo.diffsql

sealed trait Key extends Definition

trait Keys { self: SqlParser =>

  object Key {

    abstract class Parser(key: String, parser: self.Parser[Any])
        extends self.Parser[Key] {
      case class Value(
        key: String,
        name: Option[Name],
        columns: List[Name])
          extends Key {
        override def toString =
          key + " " + name.getOrElse("") + " " + columns.mkString(" ")
      }
      object Value {
        def apply(
          name: Option[Name] = None,
          columns: List[Name] = Nil)
            : Value = Value(key, name, columns)
      }
      def apply(in: Input) =
        parse(parser ~> opt(value) ~ Apply(repsep(value, ",".r)) ^^ {
          case name ~ columns =>
            Value(name.map(Name.apply), columns.map(Name.apply))
        }, in)
    }

    case object Primary extends Parser("PRIMARY KEY","PRIMARY".i ~ "KEY".i)
    case object Unique extends Parser("UNIQUE KEY", "UNIQUE".i ~ "KEY".i)
    case object Index extends Parser("KEY", "KEY".i | "INDEX".i)

  }

}
