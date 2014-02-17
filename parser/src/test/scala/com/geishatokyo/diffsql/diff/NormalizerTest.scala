package com.geishatokyo.diffsql.diff

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.ast.ColumnOption.NormalKey
import com.geishatokyo.diffsql.ast.DataType
import com.geishatokyo.diffsql.ast.Table
import com.geishatokyo.diffsql.ast.CreateKey
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/17.
 */
class NormalizerTest extends FlatSpec with ShouldMatchers {

  "Default normalizer" should "aggregate index" in {

    val defs = List(
      Table("User",List(Column("id",DataType("Long",Nil),Nil)),Nil),
      CreateKey("User",Key.NormalKey(Some("hoge"),List("id"),None,None))
    )

    val tables = Normalizer.normalize(defs)

    assert(tables === List(Table("User",List(
      Column("id",DataType("Long",Nil),Nil),
      Key.NormalKey(Some("hoge"),List("id"),None,None)
    ),Nil)))

  }

  "Default normalizer" should "expand column index" in {

    val defs = List(
      Table("User",List(
        Column("id",DataType("Long",Nil),List(ColumnOption.PrimaryKey)),
        Column("name",DataType("VARCHAR",List(120)),List(ColumnOption.UniqueKey))
      ),Nil)
    )

    val tables = Normalizer.expandColumnIndex(defs)

    assert(tables === List(
      Table("User",List(
        Column("id",DataType("Long",Nil),Nil),
        Column("name",DataType("VARCHAR",List(120)),Nil),
        Key.PrimaryKey(List("id"),None,None),
        Key.UniqueKey(None,List("name"),None,None)
      ),Nil)))


  }


}