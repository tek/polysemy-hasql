module Polysemy.Hasql.Database where

import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import Polysemy.Time (Seconds (Seconds))
import Sqel.Data.Codec (Encoder, FullCodec)
import Sqel.Data.Dd (Dd, DdType)
import Sqel.Data.QuerySchema (QuerySchema (QuerySchema))
import Sqel.Data.Sql (Sql)
import Sqel.Data.TableSchema (TableSchema (TableSchema))
import Sqel.PgType (tableSchema)
import Sqel.Query (CheckedQ, checkQuery)
import Sqel.ReifyCodec (ReifyCodec)
import Sqel.ReifyDd (ReifyDd)
import Sqel.ResultShape (ResultShape)
import Sqel.Statement (plain, prepared, unprepared)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database (..))

retryingSql ::
  TimeUnit t =>
  Member Database r =>
  t ->
  Sql ->
  Sem r ()
retryingSql interval =
  Database.retry interval Nothing . Database.statement () . plain

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
  restop (Database.retry interval Nothing (Database.statement q (prepared s row params)))

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

-- TODO check projection
query ::
  ∀ res query proj table r .
  Member Database r =>
  CheckedQ query table =>
  ReifyCodec Encoder query (DdType query) =>
  ReifyDd proj =>
  ReifyCodec FullCodec proj (DdType proj) =>
  ResultShape (DdType proj) res =>
  Dd query ->
  Dd proj ->
  Dd table ->
  DdType query ->
  Sql ->
  Sem r res
query queryDd projDd tableDd q s =
  Database.statement q (unprepared s decoder (encoder ^. #encodeValue))
  where
    QuerySchema _ encoder =
      checkQuery queryDd tableDd
    TableSchema _ decoder _ =
      tableSchema projDd
