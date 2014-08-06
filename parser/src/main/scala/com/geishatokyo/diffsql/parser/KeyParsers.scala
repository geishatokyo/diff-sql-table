package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.Name
import com.geishatokyo.diffsql.ast.{CreateKey, KeyAlgorithm, KeyOrder, Key}

/**
 * Created by takeshita on 14/02/17.
 */
trait KeyParsers { self : SQLParser =>



  object KeyDef {
    val order = ("ASC" ^^^ { KeyOrder.Asc}) | ("DESC" ^^^ {KeyOrder.Desc})
    val keyAlgorithm = ("USING" ~ "BTREE" ^^^ {KeyAlgorithm.BTree}) | ("USING" ~ "HASH" ^^^ {KeyAlgorithm.Hash})
    val cols = "(" ~> repsep(name,",") <~ ")"
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

    def constraintSymbol = opt("CONSTRAINT" ~ opt(value)) ^^ {
      case Some(_ ~ Some(symbol)) => Some(symbol)
      case _ => None
    }

    val KeyKeywordByAlter = "ADD" ~> constraintSymbol ~ opt("UNIQUE" | "PRIMARY" ~ "KEY") <~ opt("KEY" | "INDEX")
    val KeyBodyByAlter = opt(name) ~ opt(keyAlgorithm) ~ cols ~ opt(order)

    val CreateKeyByAlter = ("ALTER" ~ "TABLE") ~> name ~ KeyKeywordByAlter ~ KeyBodyByAlter <~ opt(";") ^^ {
      case tableName ~ (symbol ~ Some("unique")) ~ (indexName ~ algo ~ columns ~ order) => {
        CreateKey(tableName,Key.UniqueKey(indexName.orElse(symbol.map(s => Name(s))),columns,order,algo))
      }
      case tableName ~ (symbol ~ Some("primary" ~ "key")) ~ (indexName ~ algo ~ columns ~ order) => {
        CreateKey(tableName,Key.PrimaryKey(columns,order,algo))
      }
      case tableName ~ (symbol ~ None) ~ (indexName ~ algo ~ columns ~ order) => {
        CreateKey(tableName,Key.NormalKey(indexName.orElse(symbol.map(s => Name(s))),columns,order,algo))
      }
    }
  }

  val key : Parser[Key] = {
    import KeyInTableDef._

    PrimaryKey | UniqueKey | NormalKey | FullTextKey
  }

  val createIndex = {
    import StandAloneKeyDef._

    CreateKeyByCreate | CreateKeyByAlter
  }

}
