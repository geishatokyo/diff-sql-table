package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.Definition
import com.geishatokyo.diffsql.ast.Comment
import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by takeshita on 2014/11/11.
 */
class CommentParserTest extends FlatSpec with Matchers{

  object commentParser extends CommentParser with SQLParser{
    override def skipComment = false
    override def createDefs: Parser[List[Definition]] = {
      comment.map(comment => List(Comment(comment)))
    }
  }

  "A block comment" should "parse" in {
    val multiLineComment = """
/*
This is a block comment.
Multi line comment is allowed.

*/
    """

    {
      val defs = commentParser.parseSql(multiLineComment)
      assert(defs == List(Comment(multiLineComment.trim)))
    }

    val singleLineComment = "/* SingleLine is also allowed */  "

    {
      val defs = commentParser.parseSql(singleLineComment)
      assert(defs == List(Comment(singleLineComment.trim)))
    }

  }
  "Inline comment" should "parse" in {

    val sharp = " #hoge"

    {
      val defs = commentParser.parseSql(sharp)
      assert(defs == List(Comment(sharp.trim)))

    }

    val doubleHyphen =
      """--fuga
        |
      """.stripMargin

    {
      val defs = commentParser.parseSql(sharp)
      assert(defs == List(Comment(sharp.trim)))

    }

  }



}
