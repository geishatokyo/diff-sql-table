package com.geishatokyo.diffsql.test

import com.geishatokyo.diffsql._

import org.scalatest._
import org.scalatest.matchers._

class ParserSpec extends FlatSpec with ShouldMatchers { self =>
  import Samples._

  "parser" should "succeed in parsing sql" in {
    SqlParser.parseSql(slick)
    SqlParser.parseSql(mysql)
    SqlParser.parseSql(sqlite)
    SqlParser.parseSql(sample1)
    SqlParser.parseSql(sample2)
    SqlParser.parseSql(sample3)
    SqlParser.parseSql(sample4)
    SqlParser.parseSql(sample5)
    SqlParser.parseSql(mysqlDocSample)
  }

  "difference of sqls" should "be only option" in {
    val result = SqlParser.diff(mysql, slick)
    // Engin is changed
    assert(result.diffs.size === 1, result) 
  }

  "difference of sqls" should "be name and supid" in {
    val result = SqlParser.diff(slick, fake)
    assert(result.nonEmpty, result)
    val diff = result.diffs(0)
    assert(diff.add.size === 2, result)
    assert(diff.drop.size === 2, result)
    assert(diff.modify.size === 1, result)
  }
  
  "parser" should "marge create index definition into table" in {
    val d = SqlParser.parseSql(withCreateIndexStatement)
    
    assert(d.size === 2,d)
    
  }
  

  "primary key and unique key" should "be abstracted from sql" in {
    val coffee1 = """CREATE TABLE `coffees` (
  `COF_NAME` varchar(254) NOT NULL,
  `SUP_ID` int(11) NOT NULL,
  PRIMARY KEY (`COF_NAME`),
  UNIQUE KEY (`SUP_ID`)
)"""
    val coffee2 = """CREATE TABLE `coffees` (
  `COF_NAME` varchar(254) NOT NULL PRIMARY KEY,
  `SUP_ID` int(11) NOT NULL UNIQUE KEY
)"""
    val result = SqlParser.diff(coffee1, coffee2)
    assert(result.isEmpty, result)
  }
  
  
  val multiSqlBefore = """CREATE TABLE User (
    id bigint PRIMARY KEY AUTO_INCREMENT,
    name Varchar(200)
  );
  CREATE TABLE Role(
    id bigint PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(20),
    category INTEGER
  );
  CREATE TABLE AAAA(
    id bigint
  );
  CREATE INDEX idx_name ON User (name);
  """
  
   val multiSqlAfter = """CREATE TABLE User (
    id bigint PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(200),
    age INTEGER
  );
  CREATE TABLE Role(
    id bigint PRIMARY KEY AUTO_INCREMENT,
    name VARCHAR(20),
    categoryId INTEGER
  );
  CREATE TABLE BBBB(
    id bigint
  );
  CREATE INDEX idx_name ON User (name,age);
  """
  
  "multiple table defs" should "get all changes" in {
  
    import SqlParser._
    val diffs = SqlParser.diff(multiSqlAfter,multiSqlBefore)
    
    assert(diffs.createTables.size === 1,diffs.createTables)
    assert(diffs.createTables(0).name === "BBBB",diffs.createTables)
    assert(diffs.dropTables.size === 1,diffs.dropTables)
    assert(diffs.dropTables(0).name === "AAAA",diffs.dropTables)
    
    assert(diffs.diffs.size === 2,diffs.diffs)
    val diffForUser = diffs.diffs.find(_.name == "User").get
    assert(diffForUser.add.size === 2,diffForUser) // column age and index idx_name
    assert(diffForUser.drop.size === 1,diffForUser) // index idx_name
    assert(diffForUser.modify.size === 0 ,diffForUser)
    
    val diffForRole = diffs.diffs.find(_.name == "Role").get
    assert(diffForRole.add.size === 1,diffForRole) // column categoryId
    assert(diffForRole.drop.size === 1,diffForRole) // column category
    assert(diffForRole.modify.size === 0,diffForRole)
  }

}

object Samples {

  val mysql = """CREATE TABLE `coffees` (
  `COF_NAME` varchar(254) NOT NULL,
  `SUP_ID` int(11) NOT NULL,
  `PRICE` double NOT NULL,
  PRIMARY KEY (`COF_NAME`)
) ENGINE=InnoDB DEFAULT CHARSET=latin1"""

  val sqlite = """CREATE TABLE t1(
t  TEXT,
nu NUMERIC,
i  INTEGER,
r  REAL,
no BLOB
)"""

  val slick = """create table `COFFEES` (`COF_NAME` VARCHAR(254) NOT NULL PRIMARY KEY,`SUP_ID` INTEGER NOT NULL,`PRICE` DOUBLE NOT NULL)"""

  val fake = """create table `COFFEES` (`COFNAME` VARCHAR(254) NOT NULL PRIMARY KEY,`SUP_ID` VARCHAR(254) NOT NULL,`PRICE` FLOAT NOT NULL)"""

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

  val sample3 = """CREATE TABLE `itemdata` (
`itemId` bigint(20) NOT NULL,
`itemType` int(11) NOT NULL,
`name` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
`thumbnail` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
`explanation` longtext CHARACTER SET utf8mb4 NOT NULL,
`created` datetime NOT NULL,
`updated` datetime NOT NULL,
PRIMARY KEY (`itemId`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8"""

  val sample4 = """
CREATE TABLE `friendlink` (
`id` bigint(20) NOT NULL AUTO_INCREMENT,
`ownerId` bigint(20) NOT NULL,
`friendId` bigint(20) NOT NULL,
`status` int(11) NOT NULL,
`friendship` int(11) NOT NULL,
`updated` datetime NOT NULL,
PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8
"""

  val sample5 = """CREATE TABLE `musicinfo` (
`id` bigint(20) NOT NULL AUTO_INCREMENT,
`musicHash` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
`title` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
`artist` varchar(100) CHARACTER SET utf8mb4 NOT NULL,
`duration` double NOT NULL,
PRIMARY KEY (`id`),
UNIQUE KEY `KEY_MusicInfo_musicHash` (`musicHash`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8
"""

  val withCreateIndexStatement = """
create table Information (
    url varchar(128) not null,
    id bigint primary key not null auto_increment,
    title varchar(128) not null,
    beginDate datetime not null
  );
create table PushInformation (
    endDate datetime not null,
    id bigint primary key not null,
    message varchar(128) not null,
    pushed boolean default false not null,
    beginDate datetime not null
  );
create index idxf5f20dbb on Information (beginDate,title); 
create unique index idx3894324 on Information (beginDate,url); 
alter table Information add constraint TAccessTokenCPK unique(title,beginDate);
"""

  val mysqlDocSample = """CREATE TABLE pet (
name VARCHAR(20),
owner VARCHAR(20),
species VARCHAR(20),
sex CHAR(1),
birth DATE,
death DATE
)"""




}
