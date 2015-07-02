package com.geishatokyo.diffsql.mysql

import java.util.Locale

import com.geishatokyo.diffsql.Name
import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.diff.{Diff, DiffSet}
import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * @author takeshita
 * @author ponkotuy
 * Date: 2014/08/01.
 */
class MySQLnizerTest extends FlatSpec with Matchers  {
  import MySQLnizerTest._

  "MySQLnizer" should "generate create table sql from Table" in {
    val table = Table(Name("Hoge"),List(
      Column(Name("hoge"),DataType("Integer",List(1)),List(ColumnOption.NotNull)),
      Key.UniqueKey(Some("uk"),List(Name("hoge")),None,None),
      Key.FullTextKey(None,List(Name("hoge"))),
      foreignKey
    ),Nil)

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

  "MySQLnizer" should "generate drop foreign key" in {
    val keys = DiffSet[Key](Nil, List(foreignKey), Nil)
    val diff = Diff("hoge_table", nilDiffSet, keys, nilDiffSet)
    val sqls = sqlnizer.keys(diff)
    assert(sqls.size == 1)
    val sql = sqls.head
    val words = sql.split(" |;").map(_.toLowerCase(Locale.US)).filter(_.nonEmpty)
    val resultWords = Array("alter", "table", "hoge_table", "drop", "foreign", "key", foreignKey.name.get.name)
    assert(words.zip(resultWords).forall { case (x, y) => x == y })
  }

}

object MySQLnizerTest {
  val ref = Key.Reference("refTable", List(Name("id")), Some(ReferenceOption.Cascade), Some(ReferenceOption.SetNull))
  val foreignKey = Key.ForeignKey(Some(Name("hoge_table_fk")), List("hoge"), ref)
  val sqlnizer = new MySQLnizer

  val nilDiffSet = DiffSet(Nil, Nil, Nil)
}
