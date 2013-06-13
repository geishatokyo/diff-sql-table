import org.scalatest._
import org.scalatest.matchers._

import sql.SqlParser
import scala.slick.driver.MySQLDriver.simple._

object Coffees extends Table[(String, Int, Double)]("COFFEES") {
  def name = column[String]("COF_NAME", O.PrimaryKey)
  def supID = column[Int]("SUP_ID")
  def price = column[Double]("PRICE")
  def * = name ~ supID ~ price
}

class ParserSpec extends FlatSpec with ShouldMatchers {

  "parser" should "succeed in parsing sql" in {
    val ddl = Coffees.ddl
    SqlParser.parseSql(ddl.createStatements.mkString) match {
      case x: SqlParser.NoSuccess => fail(x.toString)
      case _ => 
    }
  }

}
