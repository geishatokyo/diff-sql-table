package com.geishatokyo.diffsql

import org.scalatest.FlatSpec
import org.scalatest.Matchers

/**
 * Created by takeshita on 14/02/18.
 */
class ParseTest extends FlatSpec with Matchers {

  "test" should "be" in {

    val builder = DiffSQLBuilder.MySQLBuilder
    val sqlDiff = builder.build()

    val sqls = sqlDiff.diff(afterSQL,beforeSQL)

    println(sqls)

    /*assert(sqls === List("""CREATE TABLE IF NOT EXISTS UserDesc (
  id INT ,
  aaa INT
);""",
      """ALTER TABLE User ADD COLUMN name VARCHAR(100) ;""",
      """ALTER TABLE User DROP COLUMN age;""",
      """ALTER TABLE User ADD  KEY a  (name,gender) ;"""))*/
  }

  val afterSQL =
    """
      |CREATE TABLE User(
      |  id INT AUTO_INCREMENT PRIMARY KEY,
      |  name VARCHAR(100),
      |  gender Int
      |) /*!50500 partition by RANGE COLUMNS(id) (
      |  PARTITION p3 VALUES LESS THAN (200),
      |  PARTITION p5 VALUES LESS THAN (300) )*/;
      |
      |CREATE INDEX a on user (name,gender);
      |
      |CREATE TABLE UserDesc(
      |  id INT,
      |  aaa Int
      |);
      |
      |ALTER TABLE UserDesc partition by RANGE COLUMNS(id) (
      |  PARTITION p3 VALUES LESS THAN (200),
      |  PARTITION p4 VALUES LESS THAN (300)
      |);
      |
    """.stripMargin

  val beforeSQL =
    """
      |# Before sql
      |CREATE TABLE User(
      |  id INT AUTO_INCREMENT PRIMARY KEY,
      |  age INT,
      |  gender Int,
      |  hoge DAteTime DEFAULT "2011-01-01 11:00:00" NOT NULL,
      |  fuga DAteTime DEFAULT '2011-01-01' NOT NULL
      |);
      |-- Alter teble
      |/* also
      |block comment */
      |alter table User add constraint TAccessTokenCPK unique(tokenId,createIndex);
      |
      |CREATE TABLE /*hoge*/ Hoge(
      |  id INT #This column is ID
      |)
      |;
      |
      |
    """.stripMargin

}