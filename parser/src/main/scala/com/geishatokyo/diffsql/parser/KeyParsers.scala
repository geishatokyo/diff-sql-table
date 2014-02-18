package com.geishatokyo.diffsql.parser

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

    val KeyKeywordByAlter = "ADD" ~ opt("CONSTRAINT") ~> opt("UNIQUE") <~ ("KEY" | "INDEX")
    val KeyBodyByAlter = opt(name) ~ opt(keyAlgorithm) ~ cols ~ opt(order)

    val CreateKeyByAlter = ("ALTER" ~ "TABLE") ~> name ~ KeyKeywordByAlter ~ KeyBodyByAlter <~ opt(";") ^^ {
      case tableName ~ Some(_) ~ (indexName ~ algo ~ columns ~ order) => {
        CreateKey(tableName,Key.UniqueKey(indexName,columns,order,algo))
      }
      case tableName ~ None ~ (indexName ~ algo ~ columns ~ order) => {
        CreateKey(tableName,Key.NormalKey(indexName,columns,order,algo))
      }
    }
  }

  val key : Parser[Key] = {
    import KeyInTableDef._

    PrimaryKey | UniqueKey | NormalKey
  }

  val createIndex = {
    import StandAloneKeyDef._

    CreateKeyByCreate | CreateKeyByAlter
  }

}
