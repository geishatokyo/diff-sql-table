package com.geishatokyo.diffsql.diff

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.ast.DataType
import com.geishatokyo.diffsql.ast.Table
import com.geishatokyo.diffsql.ast.CreateKey
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/17.
 */
class AggregatorTest extends FlatSpec with Matchers {

  "Aggregater" should "aggregate index" in {

    val defs = List(
      Table("User",List(Column("id",DataType("Long",Nil),Nil)),Nil),
      CreateKey("User",Key.NormalKey(Some("hoge"),List("id"),None,None)),
      CreatePartition("User",HashPartition("id",4))
    )

    val tables = Aggregator.aggregate(defs)

    assert(tables === List(Table("User",List(
      Column("id",DataType("Long",Nil),Nil),
      Key.NormalKey(Some("hoge"),List("id"),None,None)
    ),List(HashPartition("id",4)))))

  }



}