package com.geishatokyo.diffsql.parser

import scala.annotation.tailrec
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import com.geishatokyo.diffsql.{Name, Definition}

/**
 * Created by takeshita on 14/02/14.
 */
trait SQLParser extends RegexParsers with SkippingParsers{

  def skipComment : Boolean = true

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
        Failure("`"+s+"' is expected but "+ found + " is found", in.drop(start - offset))
      }
    }
  }


  override protected def handleWhiteSpace(source: CharSequence, offset: Int): Int = {
    if(skipComment){
      val o1 = super.handleWhiteSpace(source,offset)
      val o2 = handleComment(source,o1)
      if(o1 != o2) handleWhiteSpace(source,o2)
      else o2
    }else{
      super.handleWhiteSpace(source,offset)
    }
  }


  def handleComment(source : CharSequence, offset : Int) : Int = {
    val o1 = handleBlockComment(source,offset)
    if(o1 != offset) o1
    else handleInlineComment(source,offset)
  }

  def handleBlockComment(source : CharSequence,offset : Int) : Int = {
    if(source.length() - offset < 4) {
      return offset
    }
    val commentStart = source.subSequence(offset,offset + 2)
    if(commentStart == "/*"){
      val s = source
      var i = offset + 2
      while((i + 1 < s.length) && !(s.charAt(i) == '*' && s.charAt(i + 1) == '/')){
        i += 1
      }
      if(i + 1 < s.length){
        i + 2
      }else{
        offset
      }
    }else {
      offset
    }
  }
  def handleInlineComment(source : CharSequence,offset : Int) : Int = {
    if(source.length() - offset < 2) {
      return offset
    }
    val commentStart = source.subSequence(offset,offset + 2)
    if(commentStart == "--" || commentStart.charAt(0) == '#'){
      val s = source
      var i = offset + 2
      while(i < s.length && (s.charAt(i) != '\n')){
        i += 1
      }
      if(i < s.length){
        i + 1
      }else{
        i
      }
    }else {
      offset
    }
  }


  // Default data structure

  def bool : Parser[Boolean] = "true" ^^^ { true } | "false" ^^^ { false }
  def digits : Parser[Int] = """-?\d+""".r ^^ { case v => v.toInt}
  def floats : Parser[Double] = """-?\d+\.\d+""".r ^^ { case v => v.toDouble}
  def nonLiteralChars : Parser[String] = """[a-zA-Z0-9_]+""".r
  def literalChars : Parser[String] = """\w+""".r

  def dataTypeChars = """[a-zA-Z0-9¥(¥)]+""".r

  def value = ("`" ~> literalChars <~ "`") | nonLiteralChars
  def name = value ^^ {
    case n => Name(n)
  }

  def stringLiteral = ("'" ~> """[^']*""".r <~ "'") | ("\"" ~> """[^"]*""".r <~ "\"")

  def allValueAsString =  (bool ^^ {
      case b => b.toString
    }) | ("""-?\d+\.\d+""".r) | (digits ^^ {
    case i => i.toString
  }) | stringLiteral | value

  def functionAsString : Parser[String] = """[a-zA-Z_0-9]+""".r ~ "(" ~ repsep(
    functionAsString | allValueAsString,",") ~ ")" ^^ {
    case a ~ b ~ args ~ d => {
      a + b + args.mkString(",") + d
    }
  }


  def createDefs : Parser[List[Definition]]

  def parseSql(s : String) : List[Definition] = parseAll(createDefs,s) match {
    case Success(result,_) => result
    case noSuccess : NoSuccess => {
      throw new Exception("Fail to parse:" + noSuccess)
    }
  }



}
