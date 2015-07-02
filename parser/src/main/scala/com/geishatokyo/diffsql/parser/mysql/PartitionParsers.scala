package com.geishatokyo.diffsql.parser.mysql

import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.parser.SQLParser

/**
 * Created by takezoux2 on 15/06/30.
 */
trait PartitionParsers { self: SQLParser =>

  def CreatePatitionByAlter = "ALTER" ~ "TABLE" ~> name ~ partition ^^ {
    case table ~ partition => CreatePartition(table,partition)
  }

  def CommentOutedPartition : Parser[Partition] = "/*!" ~ "\\d+".r ~> partition <~ "*/"

  /**
   * 関数呼び出しを含めた式を文字列としてアバウトにパースする
   */
  object expressionLike extends PairLiteralSkipParser(
    "(",")",escapeBlocks = List(
      PairLiteralSkipParser("/*","*/"), //Block comment
      PairLiteralSkipParser("#","\n"), //Inline comment
      PairLiteralSkipParser("--","\n"), //Inline comment
      PairLiteralSkipParser("'","'","\\"), //String literal
      PairLiteralSkipParser("\"","\"","\\"), //String literal,
      PairLiteralSkipParser("`","`")//Literal
    )
  ) with SkipHeadWhiteSpace


  def partition : Parser[Partition] = "PARTITION" ~ "BY" ~> (rangePartition | columnsPartition | listPartition | listColumnsPartition | hashPartition | keyPartition)

  def rangePartition = "RANGE" ~> expressionLike ~ ("(" ~> repsep(lessThanRange,",") <~ ")") ^^ {
    case expression ~ ranges => {
      RangePartition(expression,ranges)
    }
  }

  def columnsPartition = ("RANGE" ~ "COLUMNS" ~ "(" ~> repsep(name,",") <~ ")") ~ ("(" ~> repsep(lessThanRange,",") <~ ")") ^^ {
    case columns ~ ranges => {
      ColumnsPartition(columns,ranges)
    }
  }

  def listPartition = "LIST" ~> expressionLike ~ ("(" ~> repsep(listRange,",") <~ ")") ^^ {
    case expression ~ ranges => {
      ListPartition(expression,ranges)
    }
  }
  def listColumnsPartition = ("List" ~ "COLUMNS" ~> "(" ~> repsep(name,",") <~ ")") ~ ("(" ~> repsep(listRange,",") <~ ")") ^^ {
    case columns ~ ranges => {
      ListColumnsPartition(columns, ranges)
    }
  }
  def hashPartition = "HASH" ~> expressionLike ~ partitions ^^ {
    case expression ~ size => {
      HashPartition(expression, size)
    }
  }

  def keyPartition = "HASH" ~> expressionLike ~ partitions ^^ {
    case expression ~ size => {
      KeyPartition(expression, size)
    }
  }

  def lessThanRange = ("PARTITION" ~> name <~ "VALUES" ~ "LESS" ~ "THAN") ~ (expressionLike | "MAXVALUE") ~ rangeOptions ^^ {
    case name ~ value ~ ops => {
      if(value == "MAXVALUE"){
        new LessThanMaxValue(name,ops)
      }else {
        LessThanRange(name, value, ops)
      }
    }
  }

  def listRange = ("PARTITION" ~> name <~ "VALUES" ~ "IN") ~ expressionLike ~ rangeOptions ^^ {
    case name ~ value ~ ops => {
      ListRange(name,value,ops)
    }
  }

  def partitions = "PARTITIONS" ~> digits

  def rangeOptions = rep(
    engine | comment | dataDirectory | indexDirectory |
    maxRows | minRows | tableSpace | nodeGroup
  )


  def engine = opt("STORAGE") ~ "ENGINE" ~ opt("=") ~> value ^^ {
    case name => RangeOption.Engine(name)
  }
  def comment = "COMMENT" ~ opt("=") ~> value ^^ {
    case c => RangeOption.Comment(c)
  }

  def dataDirectory = "DATA" ~ "DIRECTORY" ~ opt("=") ~> value ^^ {
    case dir => RangeOption.DataDirectory(dir)
  }
  def indexDirectory = "INDEX" ~ "DIRECTORY" ~ opt("=") ~> value ^^ {
    case dir => RangeOption.IndexDirectory(dir)
  }
  def maxRows = "MAX" ~ "ROWS" ~ opt("=") ~> digits ^^ {
    case size => RangeOption.MaxRows(size)
  }

  def minRows = "MIN" ~ "ROWS" ~ opt("=") ~> digits ^^ {
    case size => RangeOption.MaxRows(size)
  }
  def tableSpace = "TABLESPACE" ~ opt("=") ~> value ^^ {
    case name => RangeOption.TableSpace(name)
  }

  def nodeGroup = "NODEGROUP" ~ opt("=") ~> value ^^ {
    case id => RangeOption.NodeGroup(id)
  }



}
