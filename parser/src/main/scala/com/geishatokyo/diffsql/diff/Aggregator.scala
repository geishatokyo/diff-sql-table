package com.geishatokyo.diffsql.diff

import com.geishatokyo.diffsql.Definition
import com.geishatokyo.diffsql.ast.{CreatePartition, CreateKey, Table}

/**
 * Created by takeshita on 2014/05/27.
 */
trait Aggregator {

  def aggregate( definitions : List[Definition]) : List[Table]

}


object Aggregator extends Aggregator{

  def aggregate( definitions : List[Definition]) : List[Table] = {
    aggregateKeyAndPartition(definitions)
  }

  /**
   * CreateIndex,CreatePartition構文を、CreateTable文の中に入れ込む
   * @param definitions
   * @return
   */
  def aggregateKeyAndPartition(definitions : List[Definition]) = {
    val keys = definitions.collect({
      case k : CreateKey => k
    }).groupBy(_.table)

    val partitions = definitions.collect({
      case p : CreatePartition => p.table -> p.partition
    }).toMap

    val tables = definitions.collect({
      case t : Table => t
    })

    val aggregated = tables.map( t => {
      val t2 = keys.get(t.name) match{
        case Some(indexes) => t ++ indexes.map(_.key)
        case None => t
      }
      partitions.get(t2.name) match{
        case Some(p) => t2 + p
        case None => t2
      }
    })

    val total = keys.mapValues(_.size).map(_._2).sum + partitions.size + tables.size
    if(total == definitions.size){
      aggregated
    }else{
      println("Definition size is not match.")
      aggregated
    }
  }

}
