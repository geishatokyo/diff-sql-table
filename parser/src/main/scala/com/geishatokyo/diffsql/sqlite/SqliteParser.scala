package com.geishatokyo.diffsql.sqlite

import com.geishatokyo.diffsql.ast.ColumnOption
import com.geishatokyo.diffsql.ast.ColumnOption.SimpleCO
import com.geishatokyo.diffsql.parser._

/**
 * Created by takezoux2 on 14/12/09.
 */
class SqliteParser extends SQLParser with TableParsers with ColumnParsers with KeyParsers with DataTypeParsers {


  object sqlite{

    case object SqliteAutoIncrement extends SimpleCO("AUTOINCREMENT")

    val AutoIncrement = "AUTOINCREMENT" ^^^{SqliteAutoIncrement}
  }

  override def columnOption = {
    super[ColumnParsers].columnOption | sqlite.AutoIncrement
  }


  def createDefs = rep( createTable | createIndex)
}
