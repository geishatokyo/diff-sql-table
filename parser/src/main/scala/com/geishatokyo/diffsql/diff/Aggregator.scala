package com.geishatokyo.diffsql.diff

import com.geishatokyo.diffsql.Definition
import com.geishatokyo.diffsql.ast.{CreateKey, Table}

/**
 * Created by takeshita on 2014/05/27.
 */
trait Aggregator {

  def aggregate( definitions : List[Definition]) : List[Table]

}


object Aggregator extends Aggregator{

  def aggregate( definitions : List[Definition]) : List[Table] = {
    aggregateKey(definitions)
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

}
