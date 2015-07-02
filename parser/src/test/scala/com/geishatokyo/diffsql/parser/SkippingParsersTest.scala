package com.geishatokyo.diffsql.parser

import org.scalatest.{Matchers, FlatSpec}

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by takezoux2 on 15/07/02.
 */
class SkippingParsersTest extends FlatSpec with Matchers{

  object SkipToPairLiteral extends SQLParserForTest[String] with SkippingParsers{
    override def rootParser = {
      PairLiteralSkipParser("(",")","$",List(
        PairLiteralSkipParser("'","'","\\",Nil)
      ))
    }
  }


  "Pair lieral parser" should "parse to close literal" in {

    assert(SkipToPairLiteral.testParse("(Hoge )") == "Hoge ")
    assert(SkipToPairLiteral.testParse("(Hoge(aaa))") == "Hoge(aaa)")
    assert(SkipToPairLiteral.testParse("(With escape $) )") == "With escape $) ")

    assert(SkipToPairLiteral.testParse("(With other literal '\\')' )") == "With other literal '\\')' ")


    assert(SkipToPairLiteral.checkParse("(No close literal() ").isLeft)

  }


}
