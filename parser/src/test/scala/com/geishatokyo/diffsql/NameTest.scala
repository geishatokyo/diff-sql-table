package com.geishatokyo.diffsql

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * Created by takeshita on 2014/08/01.
 */
class NameTest extends FlatSpec with Matchers  {


  "Name" should "match case insensitive" in {
    assert(Name("INT") == Name("int"))
    assert(Name("sTrInG") == Name("StRiNg"))
  }
  "Name" should "compare with String" in {
    assert(Name("Int") == "int")
    assert(Name("long") != "int")
  }

}
