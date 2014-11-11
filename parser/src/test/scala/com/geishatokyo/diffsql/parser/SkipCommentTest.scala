package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.Definition
import com.geishatokyo.diffsql.ast.Comment
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by takeshita on 2014/11/11.
 */
class SkipCommentTest extends FlatSpec with Matchers {

  object LitralParser extends SQLParser{
    override def createDefs: Parser[List[Definition]] = {
      ("Add" ~ "\\d+".r ~ "and" ~ "\\d+".r).map(v => {
        List(Comment((v._1._1._2.toInt + v._2.toInt).toString))
      })
    }
  }


  "Comment inside any literal" should "be skipped" in {
    assert(LitralParser.parseSql("/*hoge*/Add 2/*hoge*/ and 4") == List(Comment("6")))
    assert(LitralParser.parseSql(
      """-- hoge
Add 2/*hoge*/ and 12""".stripMargin) == List(Comment("14")))
  }

}
