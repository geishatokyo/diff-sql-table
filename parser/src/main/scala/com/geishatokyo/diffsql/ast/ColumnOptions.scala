package com.geishatokyo.diffsql.ast

import com.geishatokyo.diffsql.util.StringUtil


/**
 * Created by takeshita on 14/02/14.
 */
trait ColumnOption {

}



object ColumnOption{

  abstract class SimpleCO(val name : String) extends ColumnOption{
    override def hashCode(): Int = name.toLowerCase().hashCode

    override def equals(obj: scala.Any): Boolean = obj match{
      case s : SimpleCO => s.name.toLowerCase == name.toLowerCase
      case s : String => name.toLowerCase == s.toLowerCase
      case _ => false
    }
    override def toString: String = name
  }

  trait Value
  case class IntValue(value : Long) extends Value{
    override def toString: String = value.toString
  }
  case class StringValue(value : String) extends Value{
    override def toString: String = "'" + StringUtil.escape(value) + "'"
  }
  case class FloatValue(value : Double) extends Value{
    override def toString : String = value.toString
  }
  case class BoolValue(value : Boolean) extends Value{
    override def toString: String = value.toString
  }
  case object NullValue extends Value{
    override def toString: String = "NULL"
  }

  case object Null extends SimpleCO("NULL")
  case object NotNull extends SimpleCO("NOT NULL")
  case object AutoIncrement extends SimpleCO("AUTO_INCREMENT")
  case object NormalKey extends SimpleCO("KEY")
  case object PrimaryKey extends SimpleCO("PRIMARY KEY")
  case object UniqueKey extends SimpleCO("UNIQUE KEY")
  case class Default(value : Value) extends ColumnOption{
    override def toString: String = "DEFAULT " + value
  }
  object Default{
    def apply(v : Int) : Default = Default(IntValue(v))
    def apply(v : Long) : Default = Default(IntValue(v))
    def apply(v : String) : Default = Default(StringValue(v))
    def apply(v : Double) : Default = Default(FloatValue(v))
    def apply(v : Boolean) : Default = Default(BoolValue(v))
  }



  case class Charset(charset : String) extends ColumnOption{
    override def toString: String = "CHARACTER SET " + charset
  }

}