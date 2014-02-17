package com.geishatokyo.diffsql

import com.geishatokyo.diffsql.diff.{Normalizer, StandardDifferencer, Differencer}
import com.geishatokyo.diffsql.parser.SQLParser
import com.geishatokyo.diffsql.mysql.{MySQLnizer, MySQLParser}

/**
 * Created by takeshita on 14/02/17.
 */
class DiffSQLBuilder {

  var sqlParser : SQLParser = new MySQLParser
  var normalizer : Normalizer = Normalizer
  var differencer : Differencer = new StandardDifferencer()
  var sqlnizer : SQLnizer = new MySQLnizer()


  def build() = {
    new DiffSQL(sqlParser,normalizer,differencer,sqlnizer)
  }

}
