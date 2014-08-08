package com.geishatokyo.diffsql.diff

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.ast.DataType
import com.geishatokyo.diffsql.ast.Table
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/17.
 */
class StandardDifferencerTest extends FlatSpec with Matchers {

  "StandardDifferencer" should "diff columns" in {

    implicit val eq = DataTypeEquality.OnlyName

    val table1 = Table("User",List(Column("id",DataType("BIGINT"),List(ColumnOption.PrimaryKey)),Column("name",DataType("INT"))))
    val table2 = Table("user",List(Column("id",DataType("BigInt")),Column("gender",DataType("INT"))))

    implicit val differencer  = new StandardDifferencer()

    val diff = table1 -- table2


    val col = diff.columns

    col.add == List(Column("name",DataType("INT")))
    col.remove == List(Column("gender",DataType("INT")))
    col.alter == List(Column("id",DataType("BIGINT"),List(ColumnOption.PrimaryKey)))



  }

}