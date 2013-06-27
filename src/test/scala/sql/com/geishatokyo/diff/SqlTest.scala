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

  val sample1 = """CREATE TABLE `musicinfo` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `musicHash` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
  `title` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
  `artist` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
  `duration` double NOT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `KEY_MusicInfo_musicHash` (`musicHash`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8"""

  val sample2 = """CREATE TABLE `friendlink` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `ownerId` bigint(20) NOT NULL,
  `friendId` bigint(20) NOT NULL,
  `status` int(11) NOT NULL,
  `friendship` int(11) NOT NULL,
  `updated` datetime NOT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8
"""

  "parser" should "succeed in parsing sql" in {
    assert(SqlParser.parseSql(slick).successful, slick)
    assert(SqlParser.parseSql(mysql).successful, mysql)
    assert(SqlParser.parseSql(sample1).successful, SqlParser.parseSql(sample1))
    assert(SqlParser.parseSql(sample2).successful, SqlParser.parseSql(sample2))
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
