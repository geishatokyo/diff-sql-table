# What is this?

This library makes alter table sql from 2 create table sql.


# Sample code 


## Parsing

    import com.geishatokyo.diffsql.SqlParser

    val createTableStructure = SqlParser.parseSql( yourSql )

## Diffing


    import com.geishatokyo.diffsql.SqlParser

    val alters = SqlParser.diff(oldSql,newSql)

    println(diff.toString())

or

    import com.geishatokyo.diffsql.SqlParser

    val oldTableStructure = SqlParser.parseSql(oldSql)
    val newTableStructure = SqlParser.parseSql(newSql)

    val alters = oldTableStructure alter newTableStructure

    println(diff.toString())
