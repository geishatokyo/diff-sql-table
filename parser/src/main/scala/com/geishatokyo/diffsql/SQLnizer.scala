package com.geishatokyo.diffsql

import com.geishatokyo.diffsql.ast.Table
import com.geishatokyo.diffsql.diff.Diff

/**
 * Created by takeshita on 14/02/17.
 */
trait SQLnizer {
  var createTable_? = true
  var dropTable_? = false

  def toCreateTable( table : Table) : String
  def toDropTable(table : Table) : String

  def toAlterSQL( diff : Diff) : List[String]
}
