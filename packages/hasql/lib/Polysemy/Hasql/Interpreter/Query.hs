module Polysemy.Hasql.Interpreter.Query where

import qualified Hasql.Statement as Hasql
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Db
import Polysemy.Db.Effect.Query (Query (Query))
import Sqel (DdType, ResultShape, Sqel, Statement, from, select, where_)
import Sqel.Exts (Check1)
import qualified Sqel.Syntax as Sqel
import Sqel.Syntax (project, query)

import Polysemy.Hasql.Class.RunStatement (RunStatement (runStatement))
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Effect.DbTable (DbTable)

interpretQueryUnsafe ::
  ∀ result result' query r .
  Member (Database !! DbError) r =>
  (result -> result') ->
  Hasql.Statement query result ->
  InterpreterFor (Query query result' !! DbError) r
interpretQueryUnsafe f statement =
  interpretResumable \ (Query param) -> restop (Database.statement param (f <$> statement))

interpretQueryStatementWith ::
  ∀ result result' proj query tables r .
  RunStatement tables r =>
  ResultShape proj result =>
  (result -> result') ->
  Statement tables query proj ->
  InterpreterFor (Query query result' !! DbError) r
interpretQueryStatementWith f statement =
  interpretResumable \ (Query q) -> f <$> runStatement True q statement

interpretQueryStatement ::
  ∀ result proj query tables r .
  RunStatement tables r =>
  ResultShape proj result =>
  Statement tables query proj ->
  InterpreterFor (Query query result !! DbError) r
interpretQueryStatement =
  interpretQueryStatementWith id

interpretQueryProjWith ::
  ∀ result result' query proj table r .
  Check1 table proj =>
  Check1 table query =>
  ResultShape (DdType proj) result =>
  Member (DbTable (DdType table) !! DbError) r =>
  (result -> result') ->
  Sqel query ->
  Sqel table ->
  Sqel proj ->
  InterpreterFor (Query (DdType query) result' !! DbError) r
interpretQueryProjWith f q t pr =
  interpretQueryStatementWith f Sqel.do
    frags <- project pr (query q t)
    select frags.projection
    from frags.table
    where_ frags.query

interpretQueryProj ::
  ∀ result query proj table r .
  Check1 table proj =>
  Check1 table query =>
  ResultShape (DdType proj) result =>
  Member (DbTable (DdType table) !! DbError) r =>
  Sqel query ->
  Sqel table ->
  Sqel proj ->
  InterpreterFor (Query (DdType query) result !! DbError) r
interpretQueryProj q t pr =
  interpretQueryStatement Sqel.do
    frags <- project pr (query q t)
    select frags.projection
    from frags.table
    where_ frags.query

interpretQueryWith ::
  ∀ result result' query table r .
  Check1 table query =>
  ResultShape (DdType table) result =>
  Member (DbTable (DdType table) !! DbError) r =>
  (result -> result') ->
  Sqel query ->
  Sqel table ->
  InterpreterFor (Query (DdType query) result' !! DbError) r
interpretQueryWith f q t =
  interpretQueryStatementWith f Sqel.do
    frags <- query q t
    select frags.table
    from frags.table
    where_ frags.query

interpretQuery ::
  ∀ result query table r .
  Check1 table query =>
  ResultShape (DdType table) result =>
  Member (DbTable (DdType table) !! DbError) r =>
  Sqel query ->
  Sqel table ->
  InterpreterFor (Query (DdType query) result !! DbError) r
interpretQuery q t =
  interpretQueryStatement Sqel.do
    frags <- query q t
    select frags.table
    from frags.table
    where_ frags.query

queryVia ::
  (q1 -> Sem (Stop DbError : r) q2) ->
  (r2 -> Sem (Stop DbError : r) r1) ->
  Sem (Query q1 r1 !! DbError : r) a ->
  Sem (Query q2 r2 !! DbError : r) a
queryVia transQ transR =
  interpretResumable \case
    Query param -> do
      q2 <- raiseUnder (transQ param)
      r2 <- restop (Db.query q2)
      raiseUnder (transR r2)
  . raiseUnder

mapQuery ::
  (q1 -> Sem (Stop DbError : r) q2) ->
  Sem (Query q1 result !! DbError : r) a ->
  Sem (Query q2 result !! DbError : r) a
mapQuery f =
  queryVia f pure
