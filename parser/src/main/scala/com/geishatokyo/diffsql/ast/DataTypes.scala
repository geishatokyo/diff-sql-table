package com.geishatokyo.diffsql.ast

import com.geishatokyo.diffsql.Name

/**
 * Created by takeshita on 14/02/14.
 */
case class DataType(name : Name, args : List[Int] = Nil){

  def length : Int = args.head

  def precision : Tuple2[Int,Int] = args(0) -> args(1)

  def ===(dataType : DataType)(implicit eq : DataTypeEquality) = {
    if(dataType == null) false
    else eq.equal(this,dataType)
  }
  def !==(dataType : DataType)(implicit eq : DataTypeEquality) = {
    !(this === dataType)
  }

  override def toString: String = {
    if(args.isEmpty){
      name.name
    }else{
      s"${name.name}(${args.mkString(",")})"
    }
  }
}
object DataType{

  implicit def fromString( simpleType : String) = DataType(simpleType)

}


trait DataTypeEquality{
  def normalize(d: DataType): DataType = d
  def equal(d1 : DataType , d2 : DataType) : Boolean = {
    managedEqual( normalize(d1),normalize(d2))
  }

  /**
   * Normalized data types are passed.
   * @param d1
   * @param d2
   * @return
   */
  def managedEqual(d1 : DataType,d2 : DataType) : Boolean

}

object DataTypeEquality{

  trait OnlyName extends DataTypeEquality{
    def managedEqual(d1: DataType, d2: DataType) = {
      d1.name.toLowerCase() == d2.name.toLowerCase
    }
  }
  object OnlyName extends OnlyName
  trait NameAndLength extends DataTypeEquality{
    def managedEqual(d1: DataType, d2: DataType): Boolean = {
      d1.name.toLowerCase == d2.name.toLowerCase && {
        d1.args == d2.args ||
        d1.args.isEmpty || d2.args.isEmpty
      }
    }
  }
  object NameAndLength extends NameAndLength

}