package com.geishatokyo.diffsql.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.geishatokyo.diffsql.ast.DataType

/**
 * Created by takeshita on 14/02/17.
 */
class DataTypeParserTest extends FlatSpec with ShouldMatchers {

  object DataTypeParser extends SQLParserForTest[DataType] with DataTypeParsers{
    def rootParser = dataType
  }

  "DataTypeParser" should "parse simple" in {
    assert(DataTypeParser.testParse("INT") == DataType("INT",Nil))
    assert(DataTypeParser.testParse("LONGTEXT") == DataType("LONGTEXT",Nil))
    assert(DataTypeParser.testParse("FLOAT") == DataType("FLOAT",Nil))
  }



}