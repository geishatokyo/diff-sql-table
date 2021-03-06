package com.geishatokyo.diffsql.diff

import com.geishatokyo.diffsql.{Name, Definition}
import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.ast.ColumnOption.PrimaryKey
import com.geishatokyo.diffsql.ast.CreateKey
import com.geishatokyo.diffsql.ast.Table
import scala.Some
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/17.
 */
trait Normalizer {
  def normalize( tables : List[Table]) : List[Table]

  def +(other : Normalizer) : Normalizer = {
    AndNormalizer(this,other)
  }

}
case class AndNormalizer( first : Normalizer,second : Normalizer) extends Normalizer{
  override def normalize(tables : List[Table]): List[Table] = {
    second.normalize( first.normalize(tables) )
  }
}

object Normalizer{


  /**
   * カラムに付けられたIndex情報をカラム情報と、Index情報に分離
   */
  object SeparateColumnIndex extends Normalizer {
    def normalize(tables : List[Table]) = {
      tables.map(t => {
        val indexedColumns = t.fields.collect({
          case c : Column if c.getKeyType.isDefined => c
        })
        indexedColumns.foldLeft(t)({
          case (table,column) => {
            val keyType = column.getKeyType.get
            val key = Key(keyType,List(column.name))
            table -+ (column -> (column - column.getKeyOption.get)) + key
          }
        })
      })
    }
  }

  /**
   * NotNullがデフォルトで付けられるカラムにNotNullを付ける
   * @param types
   * @param eq
   */
  case class AddNotNullAsDefault(types : DataType*)(implicit eq : DataTypeEquality) extends Normalizer{

    def normalize(tables : List[Table]) = {
      tables.map(table => {
        table.copy(fields = table.fields.map({
          case c@Column(name,dataType,options)
              if types.exists(t => t === dataType) => {
            if(c.options.contains(ColumnOption.NotNull) || c.options.contains(ColumnOption.Null)) {
              c
            }else {
              c.copy(options = ColumnOption.NotNull :: c.options)
            }
          }
          case c => c
        }))
      })
    }

  }

  /**
   * MySQLのSHOW CREATE TABLEで帰ってくるKey情報は、
   * 作成時に無名のIndexにしていた場合、最初のカラムの名前がつけられるため、
   * その差分を吸収するため無名のIndexの名前を補完する
   */
  case class CompleteKeyName() extends Normalizer{
    override def normalize(tables: List[Table]): List[Table] = {
      tables.map(t => {

        t.copy(fields = t.fields.map(f => f match{
          case Key.UniqueKey(None,columns,order,algorithm) =>{
            Key.UniqueKey(Some(columns(0).name),columns,order,algorithm)
          }
          case Key.NormalKey(None,columns,order,algorithm) =>{
            Key.NormalKey(Some(columns(0).name),columns,order,algorithm)
          }
          case Key.FullTextKey(None,columns) =>{
            Key.FullTextKey(Some(columns(0).name),columns)
          }
          case _ => f
        }))
      })
    }
  }



}
