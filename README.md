# What is this?

This library makes alter table sql from 2 create table sql.

# Sample code 


## Parsing

```scala
import com.geishatokyo.diffsql.SqlParser

val createTableStructure = SqlParser.parseSql( yourSql )
```

## Diffing

```scala
import com.geishatokyo.diffsql.SqlParser

val diff = SqlParser.diff(oldSql,newSql)

diff.foreach(println)
```

## Equalizer

```scala
import com.geishatokyo.diffsql.{SqlParser,Differ,StrictEqualizer}

val diff = (new SqlParser with Differ with StrictEqualizer).diff(oldSql,newSql)
```
