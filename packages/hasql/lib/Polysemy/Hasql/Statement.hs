module Polysemy.Hasql.Statement where

import Exon (exon)
import Hasql.Statement (Statement)
import Polysemy.Db.Data.DbName (DbName, unDbName)
import Sqel (Sql)
import Sqel.Exts (sqlQuote)
import Sqel.Statement (unsafeUntypedSql)

quoteName :: DbName -> Sql
quoteName =
  sqlQuote . unDbName

createDbSql ::
  DbName ->
  Sql
createDbSql (quoteName -> name) =
  [exon|create database #{name}|]

createDb ::
  DbName ->
  Statement () ()
createDb =
  unsafeUntypedSql . createDbSql

dropDbSql ::
  DbName ->
  Sql
dropDbSql (quoteName -> name) =
  [exon|drop database #{name}|]

dropDb ::
  DbName ->
  Statement () ()
dropDb =
  unsafeUntypedSql . dropDbSql
