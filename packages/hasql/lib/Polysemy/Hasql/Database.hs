module Polysemy.Hasql.Database where

import qualified Data.Map.Strict as Map
import Hasql.Connection (Connection)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import qualified Hasql.Session as Session (statement)
import Hasql.Statement (Statement)
import Polysemy (bindT, getInitialStateT, getInspectorT, inspect, runT)
import Polysemy.Db.Atomic (interpretAtomic)
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Internal.Tactics (liftT)
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds(Seconds), Time, TimeUnit)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database(..), InitDb(InitDb), hoistInitDb)
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Data.SqlCode (SqlCode)
import Polysemy.Hasql.DeriveStatement (deriveQuery)
import Polysemy.Hasql.Session (runSession)
import Polysemy.Hasql.Statement (plain, query)
import Polysemy.Hasql.Table.ResultShape (ResultShape)

type HasqlConnection =
  DbConnection Connection !! DbConnectionError

retryingSql ::
  TimeUnit t =>
  Member Database r =>
  t ->
  SqlCode ->
  Sem r ()
retryingSql interval =
  Database.runStatementRetrying interval () . plain

retryingSqlDef ::
  Member Database r =>
  SqlCode ->
  Sem r ()
retryingSqlDef =
  retryingSql (Seconds 3)

retryingQuerySql ::
  TimeUnit t =>
  ResultShape d result =>
  Members [Database !! e, Stop e] r =>
  t ->
  SqlCode ->
  Row d ->
  Params param ->
  param ->
  Sem r result
retryingQuerySql interval sql row params q =
  restop (Database.runStatementRetrying interval q (query sql row params))

retryingQuerySqlDef ::
  ∀ e d param result r .
  ResultShape d result =>
  Members [Database !! e, Stop e] r =>
  SqlCode ->
  Row d ->
  Params param ->
  param ->
  Sem r result
retryingQuerySqlDef =
  retryingQuerySql (Seconds 3)

connect ::
  Members [HasqlConnection, AtomicState (Map Text Int), Stop DbError, Embed IO] r =>
  InitDb (Sem r) ->
  Sem r Connection
connect (InitDb clientId initDb) =
  resumeHoist DbError.Connection do
    DbConnection.use \ count connection -> do
      whenM (reconnected count <$> atomicGets (Map.lookup clientId)) do
        raise (initDb connection)
        atomicModify' (at clientId ?~ count)
      pure connection
  where
    reconnected count = \case
      Just prev -> prev /= count
      Nothing -> True

resetConnection ::
  Members [DbConnection c !! e, Stop DbError] r =>
  DbError ->
  Sem r a
resetConnection err = do
  resume_ DbConnection.reset
  stop err

connectWithReset ::
  Members [HasqlConnection, AtomicState (Map Text Int), Time t dt, Stop DbError, Embed IO] r =>
  InitDb (Sem r) ->
  q ->
  Statement q o ->
  Sem r o
connectWithReset initDb q statement = do
  connection <- connect initDb
  traverseLeft resetConnection =<< runSession connection (Session.statement q statement)

retrying ::
  TimeUnit u =>
  Members [HasqlConnection, AtomicState (Map Text Int), Time t d, Stop DbError, Embed IO] r =>
  InitDb (Sem r) ->
  u ->
  q ->
  Statement q o ->
  Sem r o
retrying initDb interval q statement =
  traverseLeft recurseOnConnectionError =<< runStop (connectWithReset (hoistInitDb raise initDb) q statement)
  where
    recurseOnConnectionError = \case
      DbError.Connection _ -> do
        resume_ DbConnection.reset
        Time.sleep interval
        retrying initDb interval q statement
      e ->
        stop e

interpretDatabaseState ::
  ∀ t dt r .
  Members [HasqlConnection, AtomicState (Map Text Int), Time t dt, Embed IO] r =>
  InitDb (Sem r) ->
  InterpreterFor (Database !! DbError) r
interpretDatabaseState initDb =
  interpretResumableH \case
    Info ->
      liftT $ resumeHoist DbError.Connection DbConnection.info
    Name name ->
      pureT name
    WithInit (InitDb clientId initDbThunk) ma -> do
      s <- getInitialStateT
      ins <- getInspectorT
      initDbT <- bindT initDbThunk
      let initDbT' c = fold . inspect ins <$> interpretDatabaseState def (initDbT (c <$ s))
      raise . interpretDatabaseState (InitDb clientId initDbT') =<< runT ma
    Connect f -> do
      connection <- liftT (connect (hoistInitDb raise initDb))
      bindTSimple f connection
    RunStatement q statement ->
      pureT =<< connectWithReset (hoistInitDb (raise . raise) initDb) q statement
    RunStatementRetrying interval q statement ->
      pureT =<< retrying (hoistInitDb (raise . raise) initDb) interval q statement
    Sql param sql ->
      pureT =<< connectWithReset (hoistInitDb (raise . raise) initDb) param (deriveQuery sql)
{-# INLINE interpretDatabaseState #-}

-- |Run a 'Database' effect in terms of a Hasql 'Connection'.
-- To fully run with dependencies:
--
-- @
-- runM (interpretTimeGhc (interpretDbConnection (interpretDatabase prog)))
-- @
interpretDatabase ::
  ∀ t dt r .
  Members [HasqlConnection, Time t dt, Embed IO] r =>
  InterpreterFor (Database !! DbError) r
interpretDatabase =
  interpretAtomic mempty . interpretDatabaseState def . raiseUnder
{-# INLINE interpretDatabase #-}
