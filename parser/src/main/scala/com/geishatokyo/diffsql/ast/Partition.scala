package com.geishatokyo.diffsql.ast

import com.geishatokyo.diffsql.{Definition, Name}

/**
 * Created by takezoux2 on 15/07/02.
 */

case class CreatePartition(table: Name,partition: Partition) extends Definition


trait Partition extends TableOption{

}

case class RangePartition(expression: String,ranges: List[LessThanRange]) extends Partition
case class ColumnsPartition(columns:List[Name],ranges: List[LessThanRange]) extends Partition

case class ListPartition(expressio: String,ranges: List[ListRange]) extends Partition
case class ListColumnsPartition(columns: List[Name],ranges: List[ListRange]) extends Partition

case class HashPartition(expression: String,partitions: Int) extends Partition
case class KeyPartition(expression: String,partitions: Int) extends Partition

case class ListRange(name: Name,value: String,options: List[RangeOption]){
  override def equals(obj: scala.Any): Boolean = {
    obj match{
      case LessThanRange(name,_,_) => {
        name == this.name
      }
      case _ => {
        false
      }
    }
  }
}
case class LessThanRange(name: Name,value: String,options: List[RangeOption]) {
  override def equals(obj: scala.Any): Boolean = {
    obj match{
      case LessThanRange(name,_,_) => {
        name == this.name
      }
      case _ => {
        false
      }
    }
  }
}

class LessThanMaxValue(name: Name,options: List[RangeOption]) extends LessThanRange(name,"MAXVALUE",options)

trait RangeOption

object RangeOption{
  case class Engine(name: String) extends RangeOption
  case class Comment(name: String) extends RangeOption
  case class DataDirectory(dir: String) extends RangeOption
  case class IndexDirectory(dir: String) extends RangeOption
  case class MaxRows(size: Int) extends RangeOption
  case class MinRows(size: Int) extends RangeOption
  case class TableSpace(tableSpaceName: String) extends RangeOption
  case class NodeGroup(groupId: String) extends RangeOption
}