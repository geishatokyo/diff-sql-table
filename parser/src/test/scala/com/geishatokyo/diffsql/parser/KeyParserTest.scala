package com.geishatokyo.diffsql.parser

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.geishatokyo.diffsql.ast.{CreateKey, KeyAlgorithm, KeyOrder, Key}
import com.geishatokyo.diffsql.Name

/**
 * Created by takeshita on 14/02/17.
 */
class KeyParserTest extends FlatSpec with ShouldMatchers {


  object IndexInTableParser extends SQLParserForTest[Key] with KeyParsers{
    def rootParser = key
  }

  object StandAloneIndexParser extends SQLParserForTest[CreateKey] with KeyParsers{
    def rootParser = createIndex
  }

  "IndexParser" should "parse key in table" in {
    assert(IndexInTableParser.testParse("key hoge (name, age)") === Key.NormalKey(Some(Name("hoge")),List("name","age"),None,None))
    assert(IndexInTableParser.testParse("constraint Unique fuga (name, age) ASC") === Key.UniqueKey(Some(Name("fuga")),List("name","age"),Some(KeyOrder.Asc),None))
    assert(IndexInTableParser.testParse("constraint PRIMARY KEY USING HASH (name, age) DESC") === Key.PrimaryKey(List("name","age"),Some(KeyOrder.Desc),Some(KeyAlgorithm.Hash)))
    assert(IndexInTableParser.testParse("fullText (article,title)") === Key.FullTextKey(None,List(Name("article"),Name("title"))))
    assert(IndexInTableParser.testParse("fullText index only_article (article)") === Key.FullTextKey(Some("only_article"),List(Name("article"))))

  }


  "IndexParser" should "parse key by create index" in {
    assert(StandAloneIndexParser.testParse("Create index hoge on user (name,age)") === CreateKey("user",Key.NormalKey(Some(Name("hoge")),List("name","age"),None,None)))
    assert(StandAloneIndexParser.testParse("Create unique index hoge on user (name,age) Desc") === CreateKey("user",Key.UniqueKey(Some(Name("hoge")),List("name","age"),Some(KeyOrder.Desc),None)))

  }
  "IndexParser" should "parse key by alter table" in {
    assert(StandAloneIndexParser.testParse("ALTER table user add key hoge (name,age)") === CreateKey("user",Key.NormalKey(Some(Name("hoge")),List("name","age"),None,None)))
    assert(StandAloneIndexParser.testParse("ALTER table user add unique key (name,age)") === CreateKey("user",Key.UniqueKey(None,List("name","age"),None,None)))


  }

  "IndexParser" should "parse with symbol" in {
    assert(StandAloneIndexParser.testParse("ALTER table user add Constraint Symbol unique(name,age)") === CreateKey("user",Key.UniqueKey(Some("Symbol"),List("name","age"),None,None)))

  }


}