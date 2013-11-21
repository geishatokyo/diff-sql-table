package com.geishatokyo.diffsql

trait DataTypes { self: SqlParser =>
 
  sealed trait DataType

  object DataType {

    val charset = "CHARACTER".i ~ "SET".i ~> value ^^ ("CHARACTER SET " + _)

    abstract class Simple extends SelfParser[DataType] {
      val value: DataType
      def parser = className(getClass).dropRight(1).i ^^^ value
    }
    trait SimpleType extends Simple with DataType {
      val value = this
    }

    trait Sized extends DataType {
      val length: List[Int]
      override def toString = className(getClass) + (if (length.isEmpty) "" else length.mkString("(", ", ", ")"))
    }
    trait Constructor extends SelfParser[DataType] {
      def apply(length: List[Int]): DataType
      def parser = toString.i ~> opt(Apply(rep1("""\d+""".r))) ^^ (x => apply(x.toList.flatten.map(_.toInt)))
    }

    case object BOOLEAN extends SimpleType

    case class INT(length: List[Int]) extends DataType with Sized
    case object INT extends Constructor

    case object INTEGER extends Constructor {
      def apply(length: List[Int]) = INT(length)
    }

    case class BIGINT(length: List[Int]) extends DataType with Sized
    case object BIGINT extends Constructor

    case class CHAR(length: List[Int]) extends DataType with Sized
    case object CHAR extends Constructor {
      override def parser = super.parser <~ opt(charset)
    }
    case class VARCHAR(length: List[Int]) extends DataType with Sized
    case object VARCHAR extends Constructor {
      override def parser = super.parser <~ opt(charset)
    }
    case object UUID extends Simple { val value = CHAR(List(36)) }

    case object FLOAT extends SimpleType
    case object REAL extends Simple { val value = FLOAT }
    case object DOUBLE extends SimpleType

    case object DECIMAL extends SimpleType
    case object NUMERIC extends Simple { val value = DECIMAL }

    case object DATE extends SimpleType

    case object TIMESTAMP extends SimpleType
    case object DATETIME extends Simple { val value = TIMESTAMP }

    case object BLOB extends SimpleType
    case object BINARY extends Simple { val value = BLOB }

    case object TEXT extends SimpleType
    case object LONGTEXT extends SimpleType {
      override def parser = super.parser <~ opt(charset)
    }

  }

}
