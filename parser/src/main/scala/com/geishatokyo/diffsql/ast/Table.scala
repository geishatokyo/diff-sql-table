package com.geishatokyo.diffsql.ast

import com.geishatokyo.diffsql.{SQLnizer, Name, Definition}
import com.geishatokyo.diffsql.diff.{Diff, Differencer}

/**
 * Created by takeshita on 14/02/14.
 */
case class Table(name : Name, fields : List[TableField],options : List[TableOption] = Nil) extends Definition {

  def +( column : Column) = {
    copy(fields = fields :+ column)
  }
  def +( to : TableOption) = {
    copy(options = options :+ to)
  }
  def +( key : Key) = {
    copy(fields = fields :+ key)
  }
  def ++( fields : List[TableField]) = {
    copy(fields = this.fields ++ fields)
  }

  def replace(oldToNew : Tuple2[Column,Column]) = -+(oldToNew)
  def -+( oldToNew : Tuple2[Column,Column]) = {
    val fs = fields.map( f => {
      if(f == oldToNew._1) oldToNew._2
      else f
    })
    copy(fields = fs)
  }

  def --( before : Table)(implicit differencer : Differencer) = diff(before)

  def diff(before : Table)(implicit differencer : Differencer) : Diff = {
    differencer.diff(this,before)
  }

  def toCreateTableSQL()(implicit sqlnizer : SQLnizer) = sqlnizer.toCreateTable(this)
  def toDropTableSQL()(implicit sqlnizer : SQLnizer) = sqlnizer.toDropTable(this)

  def columns = fields.collect({
    case c : Column => c
  })
  def keys = fields.collect({
    case k : Key => k
  })

}

trait TableField

case class Column(name : Name, dataType : DataType , options : List[ColumnOption] = Nil) extends TableField{

  def +(co : ColumnOption) = {
    copy(options = options :+ co)
  }
  def -(co : ColumnOption) = {
    copy(options = options.filter(_ != co))
  }

  def ===(column : Column)(implicit eq : DataTypeEquality) = {
    column.name == this.name &&
    column.options.toSet == this.options.toSet &&
    column.dataType === this.dataType
  }
  def !==(column : Column)(implicit eq : DataTypeEquality) = !(this === column)

  def getKeyType = {
    options.collectFirst({
      case ColumnOption.PrimaryKey => KeyType.PrimaryKey
      case ColumnOption.NormalKey => KeyType.Normal
      case ColumnOption.UniqueKey => KeyType.Unique
    })
  }
  def getKeyOption = {
    options.collectFirst({
      case op@ ColumnOption.PrimaryKey => op
      case op@ColumnOption.NormalKey => op
      case op@ColumnOption.UniqueKey => op
    })
  }

}

case class CreateKey(table : Name, key : Key) extends Definition

trait Key extends TableField{
  def keyType : KeyType.Value
  def name : Option[Name]
  def columns : List[Name]
  def algorithm : Option[KeyAlgorithm.Value]
  def order : Option[KeyOrder.Value]

  def contains( key : Key) = {
    (this.columns zip key.columns).forall(p => p._1 == p._2)
  }
}

object Key{
  def apply(keyType : KeyType.Value, columns : List[Name]) = {
    keyType match{
      case KeyType.Normal => {
        NormalKey(None,columns,None,None)
      }
      case KeyType.Unique => {
        UniqueKey(None,columns,None,None)
      }
      case KeyType.PrimaryKey => {
        PrimaryKey(columns,None,None)
      }
    }
  }

  case class NormalKey(name : Option[Name], columns : List[Name],order : Option[KeyOrder.Value],algorithm : Option[KeyAlgorithm.Value] ) extends Key{
    val keyType: KeyType.Value = KeyType.Normal
  }

  case class UniqueKey(name :  Option[Name], columns : List[Name],order : Option[KeyOrder.Value],algorithm : Option[KeyAlgorithm.Value]) extends Key{
    val keyType: KeyType.Value = KeyType.Unique
  }
  case class PrimaryKey(columns : List[Name],order : Option[KeyOrder.Value],algorithm : Option[KeyAlgorithm.Value]) extends Key{
    val name = None
    val keyType: KeyType.Value = KeyType.PrimaryKey
  }
}

object KeyType extends Enumeration{

  val PrimaryKey = Value
  val Unique = Value
  val Normal = Value

}

object KeyOrder extends Enumeration{
  val Asc = Value
  val Desc = Value

}

object KeyAlgorithm extends Enumeration{
  val BTree = Value
  val Hash = Value
}


