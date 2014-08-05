package com.geishatokyo.diffsql.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.geishatokyo.diffsql.ast.{DataType, Column}
import com.geishatokyo.diffsql.ast.ColumnOption._

/**
 *
 * Test for ColumnParser
 */
class ColumnParserTest extends FlatSpec with ShouldMatchers {

  object ColumnParser extends SQLParserForTest[Column] with ColumnParsers with DataTypeParsers{
    def rootParser = column
  }

  def assert( frag : String, expected : Column) : Unit = {
    val c = ColumnParser.testParse(frag)
    assert(c == expected,"ParsedColumn:" + c)
  }

  "ColumParser" should "parse simple one" in{
    assert("hoge INT", Column("hoge",DataType("INT",Nil),Nil))
    assert("hoge TExt", Column("hoge",DataType("TEXt",Nil),Nil))
    assert("`hoge` INT(2)", Column("hoge",DataType("INT",List(2)),Nil))
    assert("hoge FLOAT(3,5)", Column("hoge",DataType("Float",List(3,5)),Nil))
  }



  "ColumnParser" should "parse with options" in {
    assert("hoge INT Default 3", Column("hoge",DataType("INT",Nil),List(Default(3))))
    assert("hoge INT PRIMARY KEY NOT NULL AUTO_INCREMENT", Column("hoge",DataType("INT",Nil),
      List(PrimaryKey,NotNull,AutoIncrement)))
    assert("hoge TEXT character set utf8mb4", Column("hoge",DataType("TEXT",Nil),List(Charset("utf8mb4"))))
  }

  "Minus value" should "be set as default" in {

    assert("hoge INT DEFAULT -1", Column("hoge",DataType("INT",Nil),List(Default(-1))))
    assert("hoge DOUBLE DEFAULT -0.12", Column("hoge",DataType("DOUBLE",Nil),List(Default(-0.12))))
  }

}
