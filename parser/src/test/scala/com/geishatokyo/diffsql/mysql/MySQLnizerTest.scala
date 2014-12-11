package com.geishatokyo.diffsql.mysql

import com.geishatokyo.diffsql.Name
import com.geishatokyo.diffsql.ast._
import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * Created by takeshita on 2014/08/01.
 */
class MySQLnizerTest extends FlatSpec with Matchers  {

  "MySQLnizer" should "generate create table sql from Table" in {

    val ref = Key.Reference("refTable", List(Name("id")), Some(ReferenceOption.Cascade), Some(ReferenceOption.SetNull))
    val table = Table(Name("Hoge"),List(
      Column(Name("hoge"),DataType("Integer",List(1)),List(ColumnOption.NotNull)),
      Key.UniqueKey(Some("uk"),List(Name("hoge")),None,None),
      Key.FullTextKey(None,List(Name("hoge"))),
      Key.ForeignKey(None, List(Name("hoge")), ref)
    ))

    val sqlnizer = new MySQLnizer
    val sql = sqlnizer.toCreateTable(table)

    assert(sql.contains("CREATE"))
    assert(sql.contains("TABLE"))
    assert(sql.contains("hoge"))
    assert(sql.contains("Integer"))
    assert(sql.contains("UNIQUE"))
    assert(sql.contains("FULLTEXT"))
    assert(sql.contains("REFERENCES"))
    assert(sql.contains("refTable"))
    assert(sql.contains("(id)"))
    assert(sql.contains("ON DELETE CASCADE"))
    assert(sql.contains("ON UPDATE SET NULL"))
  }

}
