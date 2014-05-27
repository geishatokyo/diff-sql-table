package com.geishatokyo.diffsql.diff

import com.geishatokyo.diffsql.ast._
import com.geishatokyo.diffsql.ast.Column

/**
 * Created by takeshita on 14/02/17.
 */
class StandardDifferencer()(implicit val dataTypeEquality: DataTypeEquality) extends Differencer {

  def diffColumns(after: List[Column], before: List[Column])(implicit dataTypeEquality: DataTypeEquality): DiffSet[Column] = {
    val add = after.filter(c => !before.exists(_.name == c.name))
    val remove = before.filter(c => !after.exists(_.name == c.name))
    val alter = after.filter( c => {
      before.find(_.name == c.name) match{
        case Some(c2) => {
          c !== c2
        }
        case None => false
      }
    })
    DiffSet(add,remove,alter)
  }

  def diffKeys(after: List[Key], before: List[Key]): DiffSet[Key] = {
    val add = after.filter( k => !before.exists(_ == k))
    val remove = before.filter( k => !after.exists(_ == k))
    val alter = after.filter( k => {
      k.name.isDefined &&
        (before.find(k2 => k2.name == k.name) match{
        case Some(k2) => k != k2
        case None => false
      })
    })

    DiffSet(add ::: alter,remove ::: alter,Nil)
  }

  def diffTableOptions(after: List[TableOption], before: List[TableOption]): DiffSet[TableOption] = {
    val add = after.filter(o => !before.exists(_ == o))
    val remove = before.filter(o => !after.exists(_ == o))

    DiffSet(add,remove,Nil)
  }
}
