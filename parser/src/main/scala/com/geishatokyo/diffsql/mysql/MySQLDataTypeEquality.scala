package com.geishatokyo.diffsql.mysql

import com.geishatokyo.diffsql.Name
import com.geishatokyo.diffsql.ast.{DataType, DataTypeEquality}

/**
 * Created by takeshita on 2014/08/01.
 */
abstract trait MySQLDataTypeSynonym extends DataTypeEquality {
  override def normalize(d: DataType): DataType = {
    d match{
      case DataType(Name("TINYINT"),List(1)) => DataType(Name("BOOLEAN"))
      case _ => d
    }
  }
}
