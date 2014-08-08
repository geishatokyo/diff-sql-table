package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.Name
import com.geishatokyo.diffsql.ast.Key.{ Reference}
import com.geishatokyo.diffsql.ast._

/**
 * Created by takeshita on 14/02/17.
 */
trait KeyParsers { self : SQLParser =>



  object KeyDef {

    def constraintSymbol = opt("CONSTRAINT" ~ opt(value)) ^^ {
      case Some(_ ~ Some(symbol)) => Some(symbol)
      case _ => None
    }

    val order = ("ASC" ^^^ { KeyOrder.Asc}) | ("DESC" ^^^ {KeyOrder.Desc})
    val keyAlgorithm = ("USING" ~ "BTREE" ^^^ {KeyAlgorithm.BTree}) | ("USING" ~ "HASH" ^^^ {KeyAlgorithm.Hash})
    val cols = "(" ~> repsep(name,",") <~ ")"

    val referenceOption = "RESTRICT" ^^^ {ReferenceOption.Restrict} |
      "CASCADE" ^^^ {ReferenceOption.Cascade}  |
      "SET" ~ "NULL" ^^^ {ReferenceOption.Restrict}  |
      "NO" ~ "ACTION" ^^^ {ReferenceOption.NoAction}

    val referenceDefinition = "REFERENCES" ~> name ~ cols ~ opt(onDelete) ~ opt(onUpdate) ^^ {
      case tableName ~ cols ~ onDelete ~ onUpdate => Reference(tableName,cols,onDelete,onUpdate)
    }
    def onDelete = "ON" ~ "DELETE" ~> referenceOption
    def onUpdate = "ON" ~ "CREATE" ~> referenceOption
  }


  object KeyInTableDef {
    import KeyDef._

    val PrimaryKey = (opt("CONSTRAINT") ~ "PRIMARY" ~ "KEY") ~> opt(keyAlgorithm) ~ cols ~ opt(order) ^^{
      case algo ~ columnNames ~ order => Key.PrimaryKey(columnNames,order,algo)
    }

    val UniqueKey = (opt("CONSTRAINT") ~ "UNIQUE" ~ opt("KEY" | "INDEX")) ~> opt(name) ~ opt(keyAlgorithm) ~ cols ~ opt(order) ^^ {
      case name ~ algo ~ columnNames ~ order => Key.UniqueKey(name,columnNames,order,algo)
    }
    val NormalKey = ("INDEX" | "KEY") ~> opt(name) ~ opt(keyAlgorithm) ~ cols ~ opt(order) ^^ {
      case name ~ algo ~ columnNames ~ order => Key.NormalKey(name ,columnNames,order,algo)
    }

    val FullTextKey = "FULLTEXT" ~ opt("INDEX" | "KEY") ~> opt(name) ~ cols ^^ {
      case name ~ columnNames => Key.FullTextKey(name,columnNames)
    }

    val ForeignKey = opt("CONSTRAINT" ~ opt(value)) ~ "FOREIGN" ~ "KEY" ~> opt(name) ~ cols ~ referenceDefinition ^^ {
      case name ~ columns ~ ref => Key.ForeignKey(name,columns,ref)
    }


  }

  object StandAloneKeyDef {
    import KeyDef._


    val CreateKeyByCreate = ("CREATE" ~> opt("UNIQUE") <~ "INDEX") ~ name ~ opt(keyAlgorithm) ~ ("ON" ~> name) ~ cols ~ opt(order) <~ opt(";") ^^ {
      case Some(_) ~ indexName ~ algo ~ tableName ~ columns ~ order => {
        CreateKey(tableName,Key.UniqueKey(Some(indexName),columns,order,algo))
      }
      case None ~ indexName ~ algo ~ tableName ~ columns ~ order => {
        CreateKey(tableName,Key.NormalKey(Some(indexName),columns,order,algo))
      }
    }


    def KeyKeywordByAlter = "ADD" ~> constraintSymbol ~ opt("UNIQUE" | "PRIMARY" ~ "KEY") <~ opt("KEY" | "INDEX")
    def KeyBodyByAlter = opt(name) ~ opt(keyAlgorithm) ~ cols ~ opt(order)

    val CreateKeyByAlter = ("ALTER" ~ "TABLE") ~> (CreatePrimaryKeyByAlter | CreateUniqueKeyByAlter | CreateNormalIndexByAlter | CreateForeignKeyByAlter)

    def CreatePrimaryKeyByAlter = (name <~ "ADD") ~ (constraintSymbol <~ "PRIMARY" ~ "KEY") ~ opt(keyAlgorithm) ~ cols ^^ {
      case tableName ~ symbol ~ algo ~ columns => {
        CreateKey(tableName,Key.PrimaryKey(columns,None,algo))
      }
    }
    def CreateUniqueKeyByAlter = (name <~ "ADD") ~ (constraintSymbol <~ "UNIQUE" ~ opt("KEY" | "INDEX")) ~ KeyBodyByAlter ^^ {
      case tableName ~ symbol ~ (indexName ~ algo ~ columns ~ order) => {
        CreateKey(tableName,Key.UniqueKey(indexName.orElse(symbol.map(s => Name(s))),columns,order,algo))
      }
    }
    def CreateNormalIndexByAlter = (name <~ "ADD") ~ (constraintSymbol <~ opt("KEY" | "INDEX")) ~ KeyBodyByAlter ^^ {
      case tableName ~ symbol ~ (indexName ~ algo ~ columns ~ order) => {
        CreateKey(tableName, Key.NormalKey(indexName.orElse(symbol.map(s => Name(s))), columns, order, algo))
      }
    }
    def CreateForeignKeyByAlter = (name <~ "ADD") ~ (constraintSymbol <~ "FOREIGN" ~ "KEY") ~ opt(name) ~ cols ~ referenceDefinition ^^ {
      case tableName ~ symbol ~ name ~ columns ~ ref => {
        CreateKey(tableName, Key.ForeignKey(name.orElse(symbol.map(s => Name(s))),columns,ref))
      }
    }



  }

  val key : Parser[Key] = {
    import KeyInTableDef._

    PrimaryKey | UniqueKey | NormalKey | FullTextKey | ForeignKey
  }

  val createIndex = {
    import StandAloneKeyDef._

    CreateKeyByCreate | CreateKeyByAlter
  }

}
