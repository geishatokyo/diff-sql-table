package com.geishatokyo.diffsql

trait TableOptions { self: SqlParser =>

  sealed abstract class TableOption(singleton: { val key: String }, value: String) {
    override def toString = singleton.key + "=" + value
  }

  object TableOption {

    sealed abstract class Parser(val key: String, p: self.Parser[Any]) extends SelfParser[TableOption] {
      def apply(key: String): TableOption
      val parser = p ~ opt("=") ~> value ^^ apply
      def this(key: String) = this(key, key.i)
    }

    case class Engine(value: String) extends TableOption(Engine, value)
    case object Engine extends Parser("ENGINE")
    case class Charset(value: String) extends TableOption(Charset, value)
    case object Charset extends Parser("CHARACTER SET", opt("DEFAULT".i) ~ ("CHARACTER".i ~ "SET".i | "CHARSET".i))
    case class AutoIncrement(value: String) extends TableOption(AutoIncrement, value)
    case object AutoIncrement extends Parser("AUTO_INCREMENT")

  }

}
