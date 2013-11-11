package com.geishatokyo.diffsql

sealed trait ColumnOption

trait ColumnOptions { self: SqlParser =>

  object ColumnOption {

    sealed abstract class Parser(key: String, p: self.Parser[Any]) extends SelfParser[ColumnOption] with ColumnOption {
      val parser = p ^^^ this
      def this(key: String) = this(key, key.i)
      override def toString = key
    }

    case object Null extends Parser("NULL")
    case object AutoIncrement extends Parser("AUTO_INCREMENT")
    case object NotNull extends Parser("NOT NULL", "NOT".i ~ "NULL".i)
    case object PrimaryKey extends Parser("PRIMARY KEY", opt("PRIMARY".i) ~ "KEY".i) with Key {
      def create(name: String) = Key.Primary(Set(name))
    }
    case object UniqueKey extends Parser("UNIQUE KEY", "UNIQUE".i ~ opt("KEY".i)) with Key {
      def create(name: String) = Key.Unique(None, Set(name))
    }
    case class Default(value: String) extends ColumnOption{
      override def toString() = "DEFAULT " + value
    }
    case object Default extends SelfParser[ColumnOption] {
      val parser = "DEFAULT".i ~> value ^^ Default.apply
    }

    sealed trait Key {
      def create(name: String): Definition
    }

  }

}
