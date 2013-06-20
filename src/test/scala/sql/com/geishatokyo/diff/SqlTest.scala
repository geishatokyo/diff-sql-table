import com.geishatokyo.diff

import com.geishatokyo.diff.SqlParser

import org.scalatest._
import org.scalatest.matchers._

import scala.slick.driver.MySQLDriver.simple._

object Coffees extends Table[(String, Int, Double)]("COFFEES") {
  def name = column[String]("COF_NAME", O.PrimaryKey)
  def supID = column[Int]("SUP_ID")
  def price = column[Double]("PRICE")
  def * = name ~ supID ~ price
}

object Fake extends Table[(String, String, Float)]("COFFEES") {
  def name = column[String]("COFNAME", O.PrimaryKey)
  def supID = column[String]("SUP_ID")
  def price = column[Float]("PRICE")
  def * = name ~ supID ~ price
}

class ParserSpec extends FlatSpec with ShouldMatchers {

  val mysql = """CREATE TABLE `coffees` (
  `COF_NAME` varchar(254) NOT NULL,
  `SUP_ID` int(11) NOT NULL,
  `PRICE` double NOT NULL,
  PRIMARY KEY (`COF_NAME`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1"""

  val slick = Coffees.ddl.createStatements.mkString

  val fake = Fake.ddl.createStatements.mkString

  "parser" should "succeed in parsing sql" in {
    assert(SqlParser.parseSql(slick).isSuccess)
    assert(SqlParser.parseSql(mysql).isSuccess)
  }

  "difference of two sqls" should "be only option" in {
    val result = SqlParser.diff(slick, mysql)
    assert(result.isSuccess)
    assert(!result.get.contains("ADD"))
    assert(!result.get.contains("DROP"))
  }

  "difference of sql" should "be name and supid" in {
    val result = SqlParser.diff(slick, fake)
    assert(result.isSuccess)
    assert("ADD".r.findAllMatchIn(result.get).size === 2)
    assert("DROP".r.findAllMatchIn(result.get).size === 2)
  }

}
