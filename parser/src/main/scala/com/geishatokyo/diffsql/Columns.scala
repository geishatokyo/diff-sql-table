package com.geishatokyo.diffsql

trait Columns { self: SqlParser =>

  case class Column(name: Name, dataType: DataType, options: Set[ColumnOption]) extends Definition {
    override def toString = name + " " + dataType + " " + options.mkString(" ")
  }

  object Column extends SelfParser[Column] {
    val parser = value ~ dataType ~ rep(columnOption) ^^ {
      case name ~ typ ~ opts => new Column(name, typ, opts.toSet)
    }
  }

}
