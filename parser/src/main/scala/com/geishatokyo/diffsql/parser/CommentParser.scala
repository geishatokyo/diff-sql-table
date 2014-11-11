package com.geishatokyo.diffsql.parser

/**
 * If you care about comments as definitions, use this trait.
 * Becareful if you want use this trait, set SQLParser#skipComment = false
 * Created by takeshita on 2014/11/11.
 */
trait CommentParser { self : SQLParser =>

  def comment = inlineCommentParser | blockCommentParser

  val blockCommentParser : Parser[String] = new Parser[String] {
    override def apply(in: Input): ParseResult[String] = {
      val start = handleWhiteSpace(in.source,in.offset)
      if(in.source.length() - start < 4){
        return Failure("The block comment must start with /* and end with */",in)
      }
      val commentStart = in.source.subSequence(start,start + 2)
      if(commentStart == "/*"){
        val startIndex = in.offset
        val s = in.source
        var i = start + 2
        while((i + 1 < s.length) && !(s.charAt(i) == '*' && s.charAt(i + 1) == '/')){
          i += 1
        }
        if(i + 1 < s.length){
          Success(s.subSequence(start,i + 2).toString,in.drop(i + 2))
        }else{
          Failure("The block comment must end with */", in)
        }
      }else{
        Failure("The block comment must start with /*", in)
      }

    }
  }

  val inlineCommentParser : Parser[String] = new Parser[String] {
    override def apply(in: Input): ParseResult[String] = {
      val start = handleWhiteSpace(in.source,in.offset)
      if(in.source.length() - start < 1){
        return Failure("The inline comment must start with -- or #",in)
      }
      val commentStart = in.source.subSequence(start,start + 2)
      if(commentStart == "--" || commentStart.charAt(0) == '#'){
        val startIndex = in.offset
        val s = in.source
        var i = start + 2
        while( i < s.length && (s.charAt(i) != '\n') ){
          i += 1
        }
        if(i < s.length){
          Success(s.subSequence(start,i).toString,in.drop(i + 1))
        }else{
          Success(s.subSequence(start,i).toString,in.drop(i))
        }
      }else{
        Failure("The inline comment must start with -- or #", in)
      }

    }
  }


}
