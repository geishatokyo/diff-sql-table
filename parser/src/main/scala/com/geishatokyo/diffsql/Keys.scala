package com.geishatokyo.diffsql

trait Keys { self: SqlParser =>

  abstract class Key(name: String, columns: Set[Name], index: Option[Name] = None) extends Definition {
    override def toString =
      name + " " + index.getOrElse("") + " " + columns.mkString(" ")
  }

  object Key {

    abstract class Parser(p: self.Parser[Any]) extends SelfParser[Key] {
      val parser = p ~> opt(value) ~ Apply(repsep(value, ",".r)) ^^ {
        case name ~ columns =>
          apply(name.map(Name.apply), columns.map(Name.apply).toSet)
      }
      def apply(index: Option[Name], columns: Set[Name]): Key
    }

    case class Primary(columns: Set[Name]) extends Key("PRIMARY KEY", columns)
    case object Primary extends Parser("PRIMARY".i ~ "KEY".i) {
      def apply(index: Option[Name], columns: Set[Name]) = Primary(columns)
    }

    case class Unique(index: Option[Name], columns: Set[Name]) extends Key("UNIQUE KEY", columns, index)
    case object Unique extends Parser("UNIQUE".i ~ "KEY".i)

    case class Index(index: Option[Name], columns: Set[Name]) extends Key("KEY", columns, index)
    case object Index extends Parser("KEY".i | "INDEX".i)

  }

}
