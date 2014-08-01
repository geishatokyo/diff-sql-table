package com.geishatokyo.diffsql.mysql

import com.geishatokyo.diffsql.Name
import com.geishatokyo.diffsql.ast._
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

/**
 * Created by takeshita on 2014/08/01.
 */
class MySQLnizerTest extends FlatSpec with ShouldMatchers  {


  "MySQLnizer" should "generate create table sql from Table" in {

    val table = Table(Name("Hoge"),List(
      Column(Name("hoge"),DataType("Integer",List(1)),List(ColumnOption.NotNull)),
      Key.UniqueKey(Some("uk"),List(Name("hoge")),None,None),
      Key.FullTextKey(None,List(Name("hoge")))
    ))

    val sqlnizer = new MySQLnizer
    val sql = sqlnizer.toCreateTable(table)
    //println(sql)

    assert(sql.contains("CREATE"))
    assert(sql.contains("TABLE"))
    assert(sql.contains("hoge"))
    assert(sql.contains("Integer"))
    assert(sql.contains("UNIQUE"))
    assert(sql.contains("FULLTEXT"))


  }


}
