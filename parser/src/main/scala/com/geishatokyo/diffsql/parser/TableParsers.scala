package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.ast.TableOption
import com.geishatokyo.diffsql.ast.Table
import com.geishatokyo.diffsql.parser.mysql.PartitionParsers

/**
 *
 * Created by takeshita on 14/02/17.
 */
trait TableParsers { self : SQLParser with ColumnParsers with DataTypeParsers with KeyParsers with PartitionParsers =>


  object TableDef{


    def Engine = "ENGINE" ~ opt("=") ~> value ^^ {
      case value => TableOption.Engine(value)
    }

    def AutoIncrement = "AUTO_INCREMENT" ~ opt("=") ~> digits ^^ {
      case ai => TableOption.AutoIncrement(ai)
    }

    def Charset = opt("DEFAULT") ~ ( ("CHARACTER" ~ "SET") | "CHARSET" ) ~ opt("=") ~> value ^^{
      case charset => TableOption.Charset(charset)
    }

    def Partition = partition | CommentOutedPartition

  }

  def tableOption = {
    import TableDef._

    Engine | Charset | AutoIncrement
  }


  def createDefinition = column | key

  def createTable = "CREATE" ~ "TABLE" ~ opt("IF" ~ "NOT" ~ "EXISTS") ~> name ~
    "(" ~ repsep(createDefinition,",") ~ ")" ~
    repsep(tableOption,opt(",")) ~ opt(TableDef.Partition) <~ opt(";") ^^ {
    case tableName ~ "(" ~ columnAndIndexes ~ ")" ~ tableOptions ~ partition => {
      Table(tableName,columnAndIndexes,tableOptions,partition)
    }
  }


}
