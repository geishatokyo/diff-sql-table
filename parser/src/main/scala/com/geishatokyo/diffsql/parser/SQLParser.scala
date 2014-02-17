package com.geishatokyo.diffsql.parser

import scala.util.parsing.combinator.RegexParsers
import com.geishatokyo.diffsql.{Name, Definition}

/**
 * Created by takeshita on 14/02/14.
 */
trait SQLParser extends RegexParsers{

  // Make literal case insensitive
  implicit override def literal(s: String): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      val start = handleWhiteSpace(source, offset)
      var i = 0
      var j = start
      while (i < s.length && j < source.length && s.charAt(i).toUpper == source.charAt(j).toUpper) {
        i += 1
        j += 1
      }
      if (i == s.length)
        Success(source.subSequence(start, j).toString, in.drop(j - offset))
      else  {
        val found = if (start == source.length()) "end of source" else "`"+source.charAt(start)+"'"
        Failure("`"+s+"' expected but "+found+" found", in.drop(start - offset))
      }
    }
  }


  def bool : Parser[Boolean] = "true" ^^^ { true } | "false" ^^^ { false }
  def digits : Parser[Int] = """\d+""".r ^^ { case v => v.toInt}
  def floats : Parser[Double] = """\d+(\.\d)?""".r ^^ { case v => v.toDouble}
  def nonLiteralChars : Parser[String] = """[a-zA-Z0-9_]+""".r
  def literalChars : Parser[String] = """\w+""".r

  def dataTypeChars = """[a-zA-Z0-9¥(¥)]+""".r

  def value = ("`" ~> literalChars <~ "`") | nonLiteralChars
  def name = value ^^ {
    case n => Name(n)
  }

  def stringLiteral = ("'" ~> """\w+""" <~ "'")

  def createDefs : Parser[List[Definition]]

  def parseSql(s : String) : List[Definition] = parseAll(createDefs,s) match {
    case Success(result,_) => result
    case noSuccess : NoSuccess => {
      throw new Exception("Fail to parse:" + noSuccess)
    }
  }



}
