package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.ast.Key.{Reference, ForeignKey}
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.{Name}

/**
 *
 * @author takeshita
 * @author ponkotuy
 * Date: 14/02/17.
 */
class KeyParserTest extends FlatSpec with Matchers {


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
    assert(StandAloneIndexParser.testParse("ALTER table user add Constraint Symbol unique(name,age);") === CreateKey("user",Key.UniqueKey(Some("Symbol"),List("name","age"),None,None)))
    assert(StandAloneIndexParser.testParse("alter table TAccessToken add constraint TAccessTokenCPK unique(tokenId,createIndex);") === CreateKey("TAccessToken",Key.UniqueKey(Some("TAccessTokenCPK"),List("tokenId","createIndex"),None,None)))
  }

  "Foreign key in create table" should "be parsed" in {
    assert(IndexInTableParser.testParse("FOREIGN key ArticleFK (articleId) REFERENCES Article(id)") ===
      Key.ForeignKey(Some("ArticleFK"), List[Name]("articleId"), Reference("Article", List("id"), None, None)))

    assert(IndexInTableParser.testParse("Constraint symbol FOREIGN key ArticleFK (articleId) REFERENCES Article(id) ON DELETE NO ACTION") ===
      Key.ForeignKey(Some("ArticleFK"), List[Name]("articleId"), Reference("Article", List("id"), Some(ReferenceOption.NoAction), None)))

    assert(IndexInTableParser.testParse("CONSTRAINT symbol FOREIGN KEY table1FK (column1) REFERENCES table2(column2) ON UPDATE SET NULL ON DELETE CASCADE") ===
      Key.ForeignKey(Some("table1FK"), List[Name]("column1"), Reference("table2", List("column2"), Some(ReferenceOption.Cascade), Some(ReferenceOption.SetNull))))
  }

  "Foreign key in alter table" should "be parsed" in {
    assert(StandAloneIndexParser.testParse("ALTER table user add FOREIGN KEY fk (name,age) REFERENCES User (id)") ===
      CreateKey("user",ForeignKey(Some("fk"),List[Name]("name","age"),Reference("User",List[Name]("id"),None,None)))
    )
  }


}
