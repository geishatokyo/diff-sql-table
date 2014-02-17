package com.geishatokyo.diffsql.diff

import com.geishatokyo.diffsql.Definition
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
  def normalize( definitions : List[Definition]) : List[Table]
}

object Normalizer extends Normalizer{

  def normalize(definitions: List[Definition]): List[Table] = {
    val tables = aggregateKey(definitions)
    expandColumnIndex(tables)
  }

  /**
   * CreateIndex構文を、CreateTable文の中に入れ込む
   * @param definitions
   * @return
   */
  def aggregateKey(definitions : List[Definition]) = {
    val keys = definitions.collect({
      case k : CreateKey => k
    }).groupBy(_.table)
    val tables = definitions.collect({
      case t : Table => t
    })

    tables.map( t => {
      keys.get(t.name) match{
        case Some(indexes) => t ++ indexes.map(_.key)
        case None => t
      }
    })
  }

  /**
   * カラムに付けられたIndex情報をカラム情報と、Index情報に分離
   * @param tables
   * @return
   */
  def expandColumnIndex(tables : List[Table]) = {
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
