package com.geishatokyo.diffsql.ast

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * Created by takeshita on 14/02/14.
 */
class DataTypeTest extends FlatSpec with Matchers {


  "OnlyName" should "return true case insensitive/ignore length" in {
    implicit val equality = DataTypeEquality.OnlyName
    assert( DataType("INTEGER",List(100)) === DataType("INTEGER",List(100)),"Everything wrong")
    assert( DataType("VARCHAR",List(10)) === DataType("VARCHAR",List(100)),"Fail to match name")
    assert( DataType("VARCHAR",Nil) === DataType("VARCHAR",List(100)),"Fail when length is Nil")
    assert( DataType("loNg",List(22)) === DataType("Long",List(22)),"Not case insensitive")
    assert( DataType("loNg",List(1)) === DataType("LonG",List(100)),"Case insensitive but length check failed")
    assert( DataType("loNg",List(1)) === DataType("LonG",Nil),"Case insensitive but length check failed!")

  }

  "OnlyName" should "return false" in {
    implicit val equality = DataTypeEquality.OnlyName

    assert(DataType("INT",List(100)) !== DataType("Long",List(100)), "Everything wrong!!")
    assert(DataType("INT",List(10)) !== DataType("Long",List(100)), "Everything wrong!")
    assert(DataType("INT",Nil) !== DataType("Long",List(100)), "Everything wrong")

  }


}
