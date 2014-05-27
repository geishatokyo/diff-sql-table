# What is this?

This library makes alter table sql from 2 create table sql.



# How to use

    import com.geishatokyo.diffsql._

    val builder = DiffSQLBuilder.MySQLBuilder
    val sqlDiff = builder.build()

    val alterSqls = sqlDiff.diff(afterSql , beforeSql )


## Miscs

## How to modify diffing algorithm

First you implement your com.geishatokyo.diffsql.diff.Differencer.
Then you set it to builder.

    import com.geishatokyo.diffsql._

    val builder = DiffSQLBuilder.MySQLBuilder
    builder.differencer = new YourOriginalDifferencer()
    val sqlDiff = builder.build()

    val alterSqls = sqlDiff.diff(afterSql , beforeSql )

## How to modify output sql

you implement your com.geishatokyo.diffsql.SQLnizer
Then you set it to builder

    import com.geishatokyo.diffsql._

    val builder = DiffSQLBuilder.MySQLBuilder
    builder.sqlnizer = new YourOriginalSQLnizer()
    val sqlDiff = builder.build()

    val alterSqls = sqlDiff.diff(afterSql , beforeSql )


## Process

    SQL(CreateTable,CreateIndex) -SQLParser-> AST -Aggregator-> -Normalizer-> -Differencer-> Diff -SQLnizer-> SQL(AlterTable)




