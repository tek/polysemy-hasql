module Polysemy.Hasql.Statement where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.DbName (DbName, unDbName)
import Sqel.Data.Sql (Sql, sql, sqlQuote)
import Sqel.Statement (plain)

quoteName :: DbName -> Sql
quoteName =
  sqlQuote . unDbName

createDbSql ::
  DbName ->
  Sql
createDbSql (quoteName -> name) =
  [sql|create database #{name}|]

createDb ::
  DbName ->
  Statement () ()
createDb =
  plain . createDbSql

dropDbSql ::
  DbName ->
  Sql
dropDbSql (quoteName -> name) =
  [sql|drop database #{name}|]

dropDb ::
  DbName ->
  Statement () ()
dropDb =
  plain . dropDbSql
