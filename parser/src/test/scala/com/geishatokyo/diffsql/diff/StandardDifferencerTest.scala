package com.geishatokyo.diffsql.diff

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.geishatokyo.diffsql.ast.{ColumnOption, Column, DataType, Table}

/**
 * Created by takeshita on 14/02/17.
 */
class StandardDifferencerTest extends FlatSpec with ShouldMatchers {

  "StandardDifferencer" should "diff columns" in {

    val table1 = Table("User",List(Column("id",DataType("BIGINT"),List(ColumnOption.PrimaryKey)),Column("name",DataType("INT"))))
    val table2 = Table("user",List(Column("id",DataType("BigInt")),Column("gender",DataType("INT"))))

    implicit val differencer  =new StandardDifferencer()

    val diff = table1 -- table2


    val col = diff.columns

    col.add == List(Column("name",DataType("INT")))
    col.remove == List(Column("gender",DataType("INT")))
    col.alter == List(Column("id",DataType("BIGINT"),List(ColumnOption.PrimaryKey)))



  }

}