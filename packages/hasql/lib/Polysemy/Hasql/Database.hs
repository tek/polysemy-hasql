module Polysemy.Hasql.Database where

import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import Sqel (DdType, From, ResultShape, Select, SqelFor, Sql, Statement, Where, from, select, unprepared, where_)
import Sqel.Exts (BuildClauses, Check1, Dd)
import Sqel.Statement (unsafeSql, unsafeUntypedSql)
import qualified Sqel.Syntax as Sqel
import Sqel.Syntax (project, query)
import Time (Seconds (Seconds))

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database (..))

retryingSql ::
  TimeUnit t =>
  Member Database r =>
  t ->
  Sql ->
  Sem r ()
retryingSql interval =
  Database.retry interval Nothing . Database.statement () . unsafeUntypedSql

retryingSqlDef ::
  Member Database r =>
  Sql ->
  Sem r ()
retryingSqlDef =
  retryingSql (Seconds 3)

retryingQuerySql ::
  TimeUnit t =>
  ResultShape d result =>
  Members [Database !! e, Stop e] r =>
  t ->
  Sql ->
  Row d ->
  Params param ->
  param ->
  Sem r result
retryingQuerySql interval s row params q =
  restop (Database.retry interval Nothing (Database.statement q (unsafeSql s params row)))

retryingQuerySqlDef ::
  ∀ e d param result r .
  ResultShape d result =>
  Members [Database !! e, Stop e] r =>
  Sql ->
  Row d ->
  Params param ->
  param ->
  Sem r result
retryingQuerySqlDef =
  retryingQuerySql (Seconds 3)

selectQuery ::
  ∀ {extq} {extt} res (query :: Dd extq) (proj :: Dd extt) (table :: Dd extt) tag r .
  Member Database r =>
  Check1 table proj =>
  Check1 table query =>
  BuildClauses tag [Select, From, Where] =>
  ResultShape (DdType proj) res =>
  SqelFor tag query ->
  SqelFor tag proj ->
  SqelFor tag table ->
  DdType query ->
  Sem r res
selectQuery q proj table par =
  Database.statement par (unprepared statement)
  where
    statement :: Statement '[DdType table] (DdType query) (DdType proj)
    statement = Sqel.do
      frags <- project proj (query q table)
      select frags.projection
      from frags.table
      where_ frags.query
