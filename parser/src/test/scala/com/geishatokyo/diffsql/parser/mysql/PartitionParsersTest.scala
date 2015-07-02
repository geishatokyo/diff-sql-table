package com.geishatokyo.diffsql.parser.mysql

import com.geishatokyo.diffsql.Name
import com.geishatokyo.diffsql.ast.ColumnOption._
import com.geishatokyo.diffsql.ast.{Column, DataType, Table, _}
import com.geishatokyo.diffsql.mysql.MySQLParser
import com.geishatokyo.diffsql.parser.{ColumnParsers, DataTypeParsers, SQLParserForTest}
import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by takeshita on 14/02/17.
 */
class PartitionParsersTest extends FlatSpec with Matchers {


  object LessThanRangeParser extends SQLParserForTest[LessThanRange] with PartitionParsers{
    override def rootParser = {
      lessThanRange
    }
  }

  object LessThan {
    val Integer = "PARTITION p0 VALUES LESS THAN (1960)"
    val Number = "PARTITION p0 VALUES LESS THAN (23.5)"
    val String = "PARTITION p0 VALUES LESS THAN ('hoge)')"
    val Expression = "PARTITION p0 VALUES LESS THAN (UNIX_TIMESTAMP('2008-01-01 00:00:00'))"
  }
  "Less than" should "be parsed" in {
    assert(LessThanRangeParser.testParse(LessThan.Integer) == LessThanRange("p0","1960",Nil))
    assert(LessThanRangeParser.testParse(LessThan.Number) == LessThanRange("p0","23.5",Nil))
    assert(LessThanRangeParser.testParse(LessThan.String) == LessThanRange("p0","'hoge)'",Nil))
    assert(LessThanRangeParser.testParse(LessThan.Expression) == LessThanRange("p0","UNIX_TIMESTAMP('2008-01-01 00:00:00')",Nil))
  }

  object ListRangeParser extends SQLParserForTest[ListRange] with PartitionParsers{
    override  def rootParser = listRange
  }
  object ListT {
    val Mix = "PARTITION p0 VALUES IN (1960,'hoge',TO_DATE(createTime)) ENGINE = InnoDB"
  }


  "List" should "be parsed" in {
    assert(ListRangeParser.testParse(ListT.Mix) == ListRange("p0","1960,'hoge',TO_DATE(createTime)",List(RangeOption.Engine("InnoDB"))))
  }


  object PartitionParserTest extends SQLParserForTest[CreatePartition] with PartitionParsers{
    def rootParser = CreatePatitionByAlter
  }

  object Full{
    val Columns =
      """
        |ALTER TABLE table_name
        |PARTITION BY RANGE COLUMNS(column1,column2) (
        |    PARTITION p0 VALUES LESS THAN (value1-1,value1-2),
        |    PARTITION p1 VALUES LESS THAN (value2-1,value2-2),
        |    PARTITION p2 VALUES LESS THAN MAXVALUE
        |)
      """.stripMargin
  }

  "Alter table" should "define partitioning" in {
    val create = PartitionParserTest.testParse(Full.Columns)

    assert(create.table == "table_name")
    assert(create.partition == ColumnsPartition(
      List(Name("column1"),Name("column2")),
      List(
        LessThanRange(Name("p0"),"value1-1,value1-2",Nil),
        LessThanRange(Name("p1"),"value2-1,value2-2",Nil),
        new LessThanMaxValue(Name("p2"),Nil)
      )
    ))
  }


}