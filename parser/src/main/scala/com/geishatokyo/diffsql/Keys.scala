package com.geishatokyo.diffsql

trait Keys { self: SqlParser =>

  trait Key extends Definition {
    def keyType : String
    def columns : Seq[Name]
    def indexName : Option[Name]
  
    override def toString =
      keyType + " " + indexName.getOrElse("") + " " + columns.mkString("(",",",")")
  }
  
  case class CreateKey(tableName : String,indexName : String ,columns : Seq[Name],unique : Boolean) extends CreateDefinition{
    def toKeyInTableDef = if(unique){
      Key.Unique(Some(indexName),columns)
    }else{
      Key.Index(Some(indexName),columns)
    }
  
  }

  object Key {

    abstract class Parser(p: self.Parser[Any]) extends SelfParser[Key] {
      val parser = p ~> opt(value) ~ Apply(repsep(value, ",".r)) ^^ {
        case name ~ columns =>
          apply(name.map(Name.apply), columns.map(Name.apply).toSeq)
      }
      def apply(index: Option[Name], columns: Seq[Name]): Key
    }
    

    case class Primary(columns: Seq[Name]) extends Key{
      
      def keyType = "PRIMARY KEY"
      def indexName = None
    }
    case object Primary extends Parser("PRIMARY".i ~ "KEY".i) {
      def apply(index: Option[Name], columns: Seq[Name]) = Primary(columns)
    }

    case class Unique(indexName: Option[Name], columns: Seq[Name]) extends Key{
      def keyType = "UNIQUE KEY"
      override def toString() = {
        "CONSTRAINT UNIQUE " + indexName.getOrElse("") + "(" + columns.mkString(",") + ")"
      }
    }
    case object Unique extends Parser("UNIQUE".i ~ "KEY".i)

    case class Index(indexName: Option[Name], columns: Seq[Name]) extends Key{
      def keyType = "KEY"
      override def toString() = {
        "KEY " + indexName.getOrElse("") + "(" + columns.mkString(",") + ")"
      }
    }
    case object Index extends Parser("KEY".i | "INDEX".i)

  }
  
  object CreateKey{
  
    def CreateIndex = SimpleCreateIndex | AlterTableAddUnique 
    
    object SimpleCreateIndex extends SelfParser[CreateKey] {
      val parser = ("CREATE".i ~> opt("UNIQUE".i) <~ "INDEX".i) ~ value ~ ("ON".i ~> value) ~ ("(" ~> repsep(value,",") <~ ")") <~ opt(";") ^^ {
        case isUnique ~ indexName ~ tableName ~ columns => {
          CreateKey(tableName,indexName,columns.map(Name.apply),isUnique.isDefined)
        }
      }
    }
    
    object AlterTableAddUnique extends SelfParser[CreateKey]{
      val parser = "ALTER".i ~ "TABLE".i ~> value ~ ("ADD".i ~ "CONSTRAINT".i ~> 
        value) ~ ("UNIQUE".i ~ "(" ~> repsep(value,",") <~ ")" ~ opt(";")) ^^ {
        case tableName ~ indexName ~ columns => {
          CreateKey(tableName,indexName,columns.map(Name.apply),true)
        }
      }
    }
    
  }

}
