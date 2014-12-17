package com.geishatokyo.diffsql.diff

import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.ast.Table
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/17.
 */
trait Differencer {

  def dataTypeEquality : DataTypeEquality

  def diff(after: Table, before: Table) : Diff = {
    if(after.name != before.name){
      throw new Exception("Can't diff difference tables")
    }

    Diff(after.name,
      diffColumns(after.columns,before.columns)(dataTypeEquality),
      diffKeys(after.keys,before.keys),
      diffTableOptions(after.options,before.options)
    )

  }

  def diffColumns(after : List[Column],before : List[Column])(implicit dataTypeEquality : DataTypeEquality) : DiffSet[Column]
  def diffKeys(after : List[Key],before : List[Key]) : DiffSet[Key]
  def diffTableOptions(after : List[TableOption],before : List[TableOption]) : DiffSet[TableOption]
}

case class Diff(tableName : String, columns : DiffSet[Column],keys : DiffSet[Key],tableOptions : DiffSet[TableOption])

case class DiffSet[+T]( add : List[T],remove : List[T], alter : List[T])
