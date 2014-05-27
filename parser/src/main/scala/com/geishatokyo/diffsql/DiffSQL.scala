package com.geishatokyo.diffsql

import com.geishatokyo.diffsql.diff.{Aggregator, Normalizer, Differencer}
import com.geishatokyo.diffsql.parser.SQLParser

/**
 * Created by takeshita on 14/02/17.
 */
class DiffSQL(sqlParser : SQLParser,
              aggregator : Aggregator,
              normalizer : Normalizer,
              differencer : Differencer,
              sqlnizer : SQLnizer) {

  def diff(after : String, before : String) : List[String] = {

    val afterDefs = normalizer.normalize(aggregator.aggregate(sqlParser.parseSql(after)))
    val beforeDefs = normalizer.normalize(aggregator.aggregate(sqlParser.parseSql(before)))

    val createTables = afterDefs.filter(t => {
      !beforeDefs.exists(_.name == t.name)
    })
    val dropTables = beforeDefs.filter(t => {
      !afterDefs.exists(_.name == t.name)
    })

    val diffs = afterDefs.flatMap(a => {
      beforeDefs.find(_.name == a.name) match{
        case Some(b) => {
          List(differencer.diff(a,b))
        }
        case None => Nil
      }
    })

    (if(sqlnizer.createTable_?){
      createTables.map(t => sqlnizer.toCreateTable(t))
    }else Nil ) :::
      (if(sqlnizer.dropTable_?){
        dropTables.map(t => sqlnizer.toDropTable(t))
      }else Nil) :::
    diffs.flatMap(d => sqlnizer.toAlterSQL(d))



  }
}
