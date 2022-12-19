module Polysemy.Hasql.Interpreter.Query where

import Hasql.Statement (Statement)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Effect.Query (Query (Query), query)
import Sqel.Data.Dd (Dd, DdType)
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.PgType (MkTableSchema, tableSchema)
import Sqel.Query (CheckQuery, checkQuery)
import Sqel.ResultShape (ResultShape)
import Sqel.Statement (selectWhere)

import qualified Polysemy.Hasql.Effect.DbTable as DbTable
import Polysemy.Hasql.Effect.DbTable (DbTable)

interpretQuery ::
  ∀ result query proj table r .
  ResultShape proj result =>
  Member (DbTable table !! DbError) r =>
  TableSchema proj ->
  QuerySchema query table ->
  InterpreterFor (Query query result !! DbError) r
interpretQuery proj que =
  interpretResumable \ (Query param) -> restop (DbTable.statement param stmt)
  where
    stmt :: Statement query result
    stmt = selectWhere que proj

interpretQueryDd ::
  ∀ result query proj table r .
  MkTableSchema proj =>
  CheckQuery query table =>
  ResultShape (DdType proj) result =>
  Member (DbTable (DdType table) !! DbError) r =>
  Dd table ->
  Dd proj ->
  Dd query ->
  InterpreterFor (Query (DdType query) result !! DbError) r
interpretQueryDd table proj que =
  interpretQuery ps qs
  where
    qs = checkQuery que table
    ps = tableSchema proj

queryVia ::
  (q1 -> Sem (Stop DbError : r) q2) ->
  (r2 -> Sem (Stop DbError : r) r1) ->
  Sem (Query q1 r1 !! DbError : r) a ->
  Sem (Query q2 r2 !! DbError : r) a
queryVia transQ transR =
  interpretResumable \case
    Query param -> do
      q2 <- raiseUnder (transQ param)
      r2 <- restop (query q2)
      raiseUnder (transR r2)
  . raiseUnder

mapQuery ::
  (q1 -> Sem (Stop DbError : r) q2) ->
  Sem (Query q1 result !! DbError : r) a ->
  Sem (Query q2 result !! DbError : r) a
mapQuery f =
  queryVia f pure
