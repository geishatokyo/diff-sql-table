package com.geishatokyo.diffsql.sqlite

import com.geishatokyo.diffsql.Name
import com.geishatokyo.diffsql.ast.{DataType, DataTypeEquality}

/**
 *
 * Created by takeshita on 2014/08/01.
 */
abstract trait SqliteDataTypeSynonym extends DataTypeEquality {
  override def normalize(d: DataType): DataType = {
    d match{
      case DataType(Name("INT"),_) |
           DataType(Name("TINYINT"),_) |
           DataType(Name("SMALLINT"),_) |
           DataType(Name("MEDIUMINT"),_) |
           DataType(Name("BIGINT"),_) |
           DataType(Name("UNSIGNED BIG INT"),_) |
           DataType(Name("INT2"),_) |
           DataType(Name("INT8"),_) => DataType(Name("INTEGER"))
      case DataType(Name("VARCHAR"),_) => DataType(Name("TEXT"))
      case _ => d
    }
  }
}
