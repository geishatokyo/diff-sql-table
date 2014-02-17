package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.Definition

/**
 * Created by takeshita on 14/02/17.
 */
trait SQLParserForTest[T] extends SQLParser{
  def createDefs = null

  def rootParser : Parser[T]

  def testParse(sql : String) : T  = {
    parse[T](rootParser,sql) match{
      case Success(r , _) => r
      case NoSuccess(m,_) => throw new Exception("Fail to parse:" + m + " SQL:" + sql)

    }
  }
}
