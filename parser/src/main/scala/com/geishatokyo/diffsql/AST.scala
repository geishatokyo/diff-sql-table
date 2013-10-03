package com.geishatokyo.diffsql

trait Definition

case class Name(name: String) {
  override def equals(x: Any) =
    x match {
      case n: Name => name.equalsIgnoreCase(n.name)
      case _ => false
    }
  override def toString = name
}
object Name {
  implicit def nameToString(name: Name) = name.name
  implicit def stringToName(string: String) = Name(string)
}

case class Column
  (name: Name, dataType: DataType)
  (val options: Set[ColumnOption])
    extends Definition {
  override def toString =
    name + " " + dataType + " " + options.mkString(" ")
}

sealed trait Result {
  val name: Name
}

case class Table(
  name: Name,
  columns: Set[Definition],
  options: Set[TableOption]) { self =>
  def create = new Result {
    val name = self.name
    override def toString =
      "CREATE TABLE " + name + " ( \n" +
      columns.mkString("  ", ",\n  ", "") +
      "\n);"
  }
  def drop = new Result {
    val name = self.name
    override def toString =
      "DROP TABLE " + name + ";"
  }
  override def equals(x: Any) =
    super.equals(x) && (x match {
      case t: Table => name.toUpperCase == t.name.toUpperCase
    })
}

case class Diff(
  name: Name,
  add: Set[Definition],
  drop: Set[Name],
  modify: Set[Column],
  options: Set[TableOption])
    extends Result {
  override def toString = {
    val ADD = add.map("ADD " +)
    val DROP = drop.map("DROP " +)
    val MODIFY = modify.map("MODIFY " +)
    "ALTER TABLE " + name + " " +
      (ADD ++ DROP ++ MODIFY ++ options).mkString(",") +
      ";"
  }
}
