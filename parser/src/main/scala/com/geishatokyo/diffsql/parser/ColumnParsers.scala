package com.geishatokyo.diffsql.parser

import com.geishatokyo.diffsql.ast.ColumnOption
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/14.
 */
trait ColumnParsers { self : SQLParser with DataTypeParsers =>

  object ColumnOptionDef{

    val Null = "NULL" ^^^ { ColumnOption.Null}
    val NotNull = "NOT NULL" ^^^ {ColumnOption.NotNull}
    val AutoIncrement = "AUTO_INCREMENT" ^^^ {ColumnOption.AutoIncrement}
    val PrimaryKey = "PRIMARY" ~ "KEY" ^^^ { ColumnOption.PrimaryKey }
    val NormalKey = "KEY" ^^^ { ColumnOption.NormalKey }
    val UniqueKey = "UNIQUE" ~ opt("KEY") ^^^ {ColumnOption.UniqueKey}

    val Default =
        ("DEFAULT" ~> bool ^^ {case d => ColumnOption.Default(ColumnOption.BoolValue(d))}) |
          ("DEFAULT" ~> floats ^^ {case d => ColumnOption.Default(ColumnOption.FloatValue(d))}) |
          ("DEFAULT" ~> digits ^^ {case d => ColumnOption.Default(ColumnOption.IntValue(d))}) |
        ("DEFAULT" ~> stringLiteral ^^ {case d => ColumnOption.Default(ColumnOption.StringValue(d))}) |
        ("DEFAULT" ~ "NULL" ^^^ { ColumnOption.Default(ColumnOption.NullValue)})

    val CharacterSet =
      (("CHARACTER" ~ "SET") | "CHARSET") ~> value ^^ {
        case charset => ColumnOption.Charset(charset)
      }

  }

  val columnOption = {
    import ColumnOptionDef._

    Null | NotNull | AutoIncrement | PrimaryKey | UniqueKey | NormalKey | Default | CharacterSet
  }

  val column = name ~ dataType ~ rep(columnOption) ^^ {
    case name ~ dataType ~ options => {
      Column(name,dataType,options)
    }
  }


}
