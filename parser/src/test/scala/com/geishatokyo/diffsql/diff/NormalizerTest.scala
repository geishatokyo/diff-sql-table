package com.geishatokyo.diffsql.diff

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.ast.ColumnOption.NormalKey
import com.geishatokyo.diffsql.ast.DataType
import com.geishatokyo.diffsql.ast.Table
import com.geishatokyo.diffsql.ast.CreateKey
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/17.
 */
class NormalizerTest extends FlatSpec with Matchers {


  "SeparateColumnIndex normalizer" should "expand column index" in {

    val defs = List(
      Table("User",List(
        Column("id",DataType("Long",Nil),List(ColumnOption.PrimaryKey)),
        Column("name",DataType("VARCHAR",List(120)),List(ColumnOption.UniqueKey))
      ),Nil,None)
    )

    val tables = Normalizer.SeparateColumnIndex.normalize(defs)

    assert(tables === List(
      Table("User",List(
        Column("id",DataType("Long",Nil),Nil),
        Column("name",DataType("VARCHAR",List(120)),Nil),
        Key.PrimaryKey(List("id"),None,None),
        Key.UniqueKey(None,List("name"),None,None)
      ),Nil,None)))


  }

  "AddNotNullAsDefault normalizer" should "add 'not null' option to specific data type fields" in {

    implicit val eq = DataTypeEquality.OnlyName

    val defs = List(
      Table("AddNull",List(
        Column("id",DataType("Long",Nil),List(ColumnOption.PrimaryKey)),
        Column("withoutNotNull",DataType("BOOLEAN"),List()),
        Column("withNotNull",DataType("BOOLEAN"),List(ColumnOption.NotNull)),
        Column("withNull",DataType("BOOLEAN"),List(ColumnOption.Null))
      ),Nil,None)
    )

    val tables = Normalizer.AddNotNullAsDefault("Boolean").normalize(defs)

    assert(tables === List(
      Table("AddNull",List(
        Column("id",DataType("Long",Nil),List(ColumnOption.PrimaryKey)),
        Column("withoutNotNull",DataType("BOOLEAN"),List(ColumnOption.NotNull)),
        Column("withNotNull",DataType("BOOLEAN"),List(ColumnOption.NotNull)),
        Column("withNull",DataType("BOOLEAN"),List(ColumnOption.Null))
      ),Nil,None)
    ))


  }

}