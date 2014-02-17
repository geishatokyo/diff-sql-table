package com.geishatokyo.diffsql.mysql

import com.geishatokyo.diffsql.parser._

/**
 * Created by takeshita on 14/02/17.
 */

class MySQLParser extends SQLParser with TableParsers with ColumnParsers with KeyParsers with DataTypeParsers {
  def createDefs = rep( createTable | createIndex)
}

