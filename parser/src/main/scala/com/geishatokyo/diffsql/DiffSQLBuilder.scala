package com.geishatokyo.diffsql

import com.geishatokyo.diffsql.diff.{Aggregator, Normalizer, StandardDifferencer, Differencer}
import com.geishatokyo.diffsql.parser.SQLParser
import com.geishatokyo.diffsql.mysql.{MySQLDataTypeSynonym, MySQLnizer, MySQLParser}
import com.geishatokyo.diffsql.ast.{DataType, DataTypeEquality}

/**
 * Created by takeshita on 14/02/17.
 */
trait DiffSQLBuilder {

  var sqlParser : SQLParser
  var aggregator : Aggregator
  var normalizer : Normalizer
  var differencer : Differencer
  var sqlnizer : SQLnizer


  def build() = {
    new DiffSQL(sqlParser,aggregator,normalizer,differencer,sqlnizer)
  }

}

object DiffSQLBuilder{

  object MySQLBuilder extends DiffSQLBuilder{

    implicit object dataTypeEquality extends DataTypeEquality.OnlyName with MySQLDataTypeSynonym
    var sqlParser : SQLParser = new MySQLParser
    var aggregator : Aggregator = Aggregator
    /**
     * MySQLでは、BOOLEANのカラムに対して自動でNotNullオプションが追加されるため、
     * その挙動と合わせるためにNormalize時にNotNullを付加するようにしてある
     */
    var normalizer : Normalizer = Normalizer.SeparateColumnIndex + Normalizer.AddNotNullAsDefault(DataType("BOOLEAN"))
    var differencer : Differencer = new StandardDifferencer()
    var sqlnizer : SQLnizer = new MySQLnizer()

  }

}

