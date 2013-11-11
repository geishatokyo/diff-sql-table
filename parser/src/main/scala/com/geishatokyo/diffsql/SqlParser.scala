package com.geishatokyo.diffsql

import scala.util.parsing.combinator.RegexParsers

trait Result {
  val name: Name
}

case class Name(name: String) {
  override def equals(x: Any) = x match {
    case n: Name => name.equalsIgnoreCase(n.name)
    case _ => false
  }
  override def hashCode = name.toLowerCase##
  override def toString = name
}
object Name {
  implicit def nameToString(name: Name) = name.name
  implicit def stringToName(string: String) = Name(string)
}

trait Definition

trait SqlParser extends RegexParsers
    with DataTypes
    with ColumnOptions
    with Columns
    with TableOptions
    with Tables
    with Keys {

  class CaseInsensitive(string: String) {
    def i = ("(?i)" + string).r
  }
  implicit def caseInsensitive(string: String) = new CaseInsensitive(string)

  def Apply[A](p: Parser[A]) = """\(""".r ~> p <~ """\)""".r

  val value = """[\w`]+""".r

  trait SelfParser[A] extends Parser[A] {
    val parser: Parser[A]
    final def apply(input: Input) = parser(input)
  }

  val tableOption = { import TableOption._
    Engine | Charset | AutoIncrement
  }

  val columnOption = { import ColumnOption._
    NotNull | Null | PrimaryKey | UniqueKey | AutoIncrement | Default
  }

  val dinition = { import Key._
    Column | Primary | Unique | Index
  }

  val dataType = { import DataType._
    Bit | Boolean | Bool | TinyInt | SmallInt | MediumInt | Integer | Int | BigInt |
    Binary | VarBinary |
    Char | VarChar |
    Real | Double | Float |
    Decimal | Numeric |
    TinyText | Text | MediumText | LongText |
    DateTime | Date | TimeStamp | Time | Year |
    TinyBlob | Blob | MediumBlob | LongBlob
  }
  
  def createDefs = rep(Table | CreateKey.CreateIndex)

  trait CreateDefinition

  def parseSql(s: String) = parseAll(createDefs, s) match {
    case Success(result, _) => aggregateIndex(result)
    case nosuccess: NoSuccess =>
      throw new RuntimeException(nosuccess.toString)
  }
  
  /**
   * インデックスをテーブル定義の中に入れ込む
   */
  def aggregateIndex(createDefinitions : List[CreateDefinition]) = {
    val indexes = createDefinitions.collect({
      case key : CreateKey => key
    }).groupBy(_.tableName)
    
    val tables = createDefinitions.collect({
      case table : Table => {
        indexes.get(table.name) match{
          case Some(indexes) => {
            Table(table.name,table.columns ++ indexes.map(_.toKeyInTableDef),table.options)
          }
          case None => table
        }
      }
    })
    tables
  }
  

  def diff(before: Table, after: Table): Option[Diff]

  def diff(after: String, before: String): Option[Diff] =
    diff(parseSql(before).head, parseSql(after).head)

  def genSql(a: String, b: String): Set[Result] = {
    val before = parseSql(b)
    val after = parseSql(a)
    (before.map(_.name) ++ after.map(_.name)).toSet.flatMap { (name: Name) =>
      val b = before.groupBy(_.name).mapValues(_.head)
      val a = after.groupBy(_.name).mapValues(_.head)
      ((b get name, a get name): @unchecked) match {
        case (Some(before), Some(after)) =>
          diff(before, after)
        case (None, Some(after)) => Some(after.create)
        case (Some(before), None) => Some(before.drop)
      }
    }
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
      "ALTER TABLE " + name + " " + (ADD ++ DROP ++ MODIFY ++ options).mkString(",") + ";"
    }
  }

}

object SqlParser extends SqlParser with Differ with LaxEqualizer

trait Differ { self: SqlParser =>
  def diff(before: Table, after: Table): Option[Diff] = {
    val changes =
    before.columns.collect {
      case b: Column => after.columns.collect {
        case a: Column if a.name == b.name && a.dataType != b.dataType => b
      }
    }.flatten
    def remove(defs: Set[Definition]) =
    defs filter {
      case c: Column => !changes.map(_.name).contains(c.name)
      case _ => true
    }
    val add = remove(before.columns diff after.columns)
    val drop = remove(after.columns diff before.columns).collect {
      case c: Column => c.name
    }
    val ops = before.options diff after.options
    if (changes.isEmpty && add.isEmpty && drop.isEmpty && ops.isEmpty)
      None
    else
      Some(Diff(before.name, add, drop, changes, ops))
  }
}

trait LaxEqualizer { self: SqlParser =>
  def equal(x: DataType, y: DataType) = x.hashCode == y.hashCode
}

trait StrictEqualizer { self: SqlParser =>
  def equal(x: DataType, y: DataType) =
    x.hashCode == y.hashCode && x.fields == y.fields
}
