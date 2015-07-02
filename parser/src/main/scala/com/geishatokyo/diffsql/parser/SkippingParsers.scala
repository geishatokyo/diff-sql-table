package com.geishatokyo.diffsql.parser

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by takezoux2 on 15/07/02.
 */
trait SkippingParsers { self: RegexParsers =>

  /**
   * 先頭のWhiteSpaceを飛ばす機能を追加
   */
  trait SkipHeadWhiteSpace extends Parser[String] {
    abstract override def apply(in: Input) = {
      val s = handleWhiteSpace(in.source,in.offset)
      super.apply(in.drop(s - in.offset))
    }
  }

  /**
   * 対応するリテラルまでをパースする
   * リテラルのネスト、エスケープ、内部での追加リテラルをサポート
   * @param openLiteral
   * @param closeLiteral
   * @param escapeChar
   * @param escapeBlocks
   */
  case class PairLiteralSkipParser(
    openLiteral : String,
    closeLiteral : String,
    escapeChar: String = "",
    escapeBlocks: List[PairLiteralSkipParser] = Nil) extends Parser[String] {

    assert(openLiteral != null)
    assert(openLiteral.length > 0)
    assert(closeLiteral != null)
    assert(closeLiteral.length > 0)
    assert(escapeChar != null)

    override def apply(in: Input): ParseResult[String] = {

      val startStr = in.source.subSequence(in.offset,in.offset + openLiteral.length)
      if(startStr != openLiteral){
        return Failure(s"Not start with open literal:${openLiteral}",in)
      }

      val s = in.source.toString
      var openLiteralCount = 1

      var i = in.offset + openLiteral.length
      val builder = new StringBuilder
      while(i < s.length && openLiteralCount > 0) {
        lazy val escape_? = s.substring(i,i + escapeChar.length)
        if(escapeChar.length > 0 && escape_? == escapeChar){
          builder.append(s.substring(i,i + escapeChar.length + 1))
          i += escapeChar.length + 1
        }else{
          var processed = false
          val close_? = s.substring(i,i + closeLiteral.length)
          if(close_? == closeLiteral) {
            processed = true
            openLiteralCount -= 1
          }else {
            val open_? = s.substring(i, i + openLiteral.length)
            if (open_? == openLiteral) {
              processed = true
              openLiteralCount += 1
            }
          }

          if(!processed && escapeBlocks.size > 0){
            val in2 = in.drop(i - in.offset)
            escapeBlocks.view.map(p => (p,p.apply(in2))).find(_._2.successful) match{
              case Some((p,Success(s,lest))) => {
                builder.append(p.openLiteral + s + p.closeLiteral)
                i = lest.offset
              }
              case _ => {
                builder.append(s.charAt(i))
                i += 1
              }
            }
          }else{
            if (openLiteralCount > 0) {
              builder.append(s.charAt(i))
            }
            i += 1
          }
        }

      }

      if(openLiteralCount == 0){
        Success(builder.toString(),in.drop(i - in.offset))
      }else{
        Failure(s"Close literal not found:${closeLiteral}",in)
      }

    }
  }




}
