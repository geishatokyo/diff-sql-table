package com.geishatokyo.diffsql.ast

/**
 * Created by takeshita on 14/02/17.
 */
trait TableOption {

}

object TableOption{

  case class Engine(value : String) extends TableOption
  case class Charset(value : String) extends TableOption
  case class AutoIncrement(value : Long) extends TableOption



}
