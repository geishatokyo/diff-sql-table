package com.geishatokyo.diffsql.mysql

import com.geishatokyo.diffsql.parser._
import com.geishatokyo.diffsql.parser.mysql.PartitionParsers

/**
 * Created by takeshita on 14/02/17.
 */

class MySQLParser extends SQLParser with TableParsers with ColumnParsers with KeyParsers with DataTypeParsers with PartitionParsers {
  def createDefs = rep( createTable | createIndex)
}

