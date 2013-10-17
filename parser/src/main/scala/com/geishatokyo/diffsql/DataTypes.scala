package com.geishatokyo.diffsql

trait DataTypes { self: SqlParser =>
 
  sealed trait DataType

  object DataType {

    val charset = "CHARACTER".i ~ "SET".i ~> value ^^ ("CHARACTER SET " + _)
    val length = Apply("""\d+""".r) ^^ (_.toInt)

    abstract class Simple extends SelfParser[DataType] {
      val value: DataType
      def parser = className(getClass).dropRight(1).i ^^^ value
    }
    trait Self extends DataType { self: Simple =>
      val value = this
    }

    trait Constructor extends SelfParser[DataType] {
      def apply(length: Option[Int]): DataType
      def parser = toString.i ~> opt(length) ^^ apply
    }

    trait Sized {
      val length: Option[Int]
      override def toString = className(getClass) + length.map("(" + _ + ")").getOrElse("")
    }

    case object BOOLEAN extends Simple with Self

    case object INT extends Constructor with DataType {
      def apply(length: Option[Int]) = this
    }
    case object INTEGER extends Simple { val value = INT }

    case object BIGINT extends Constructor with DataType {
      def apply(length: Option[Int]) = this
    }

    case class CHAR(length: Option[Int]) extends DataType with Sized
    case object CHAR extends Constructor {
      override def parser = super.parser <~ opt(charset)
    }
    case class VARCHAR(length: Option[Int]) extends DataType with Sized
    case object VARCHAR extends Constructor {
      override def parser = super.parser <~ opt(charset)
    }
    case object UUID extends Simple { val value = CHAR(Some(36)) }

    case object FLOAT extends Simple with Self
    case object REAL extends Simple { val value = FLOAT }
    case object DOUBLE extends Simple with Self

    case object DECIMAL extends Simple with Self
    case object NUMERIC extends Simple { val value = DECIMAL }

    case object DATE extends Simple with Self

    case object TIMESTAMP extends Simple with Self
    case object DATETIME extends Simple { val value = TIMESTAMP }

    case object BLOB extends Simple with Self
    case object BINARY extends Simple { val value = BLOB }

    case object TEXT extends Simple with Self
    case object LONGTEXT extends Simple with Self {
      override def parser = super.parser <~ opt(charset)
    }

  }

}
