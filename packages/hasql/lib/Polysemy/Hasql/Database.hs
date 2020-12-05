module Polysemy.Hasql.Database where

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session (statement)
import Hasql.Statement (Statement)
import Polysemy.Db.Atomic (interpretAtomic)
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.TableStructure (TableStructure)
import Polysemy.Resume (Stop, interpretResumableH, resume, resume_, runStop, stop, type (!))
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds(Seconds), Time, TimeUnit)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database(..))
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Data.SqlCode (SqlCode)
import Polysemy.Hasql.Session (runSession)
import Polysemy.Hasql.Statement (plain)
import Polysemy.Hasql.Table (initTable)

retryingSql ::
  TimeUnit t =>
  Member Database r =>
  t ->
  SqlCode ->
  Sem r ()
retryingSql interval =
  Database.retrying Nothing interval () . plain

retryingSqlDef ::
  Member Database r =>
  SqlCode ->
  Sem r ()
retryingSqlDef =
  retryingSql (Seconds 3)

initOnReconnect ::
  Members [AtomicState Bool, Stop DbError, Embed IO] r =>
  Connection ->
  TableStructure ->
  Sem r ()
initOnReconnect connection table =
  whenM atomicGet do
    initTable connection table
    atomicPut False

connect ::
  Members [DbConnection Connection ! DbConnectionError, AtomicState Bool, Stop DbError, Embed IO] r =>
  (Connection -> Sem r ()) ->
  Sem r Connection
connect init' = do
  connection <- resume DbConnection.connect \ err -> atomicPut True *> stop (DbError.Connection err)
  connection <$ whenM atomicGet do
    init' connection
    atomicPut False

connectTable ::
  Members [DbConnection Connection ! DbConnectionError, AtomicState Bool, Stop DbError, Embed IO] r =>
  Maybe TableStructure ->
  Sem r Connection
connectTable table =
  connect (\ connection -> traverse_ (initTable connection) table)

resetConnection ::
  Members [DbConnection c ! e, AtomicState Bool, Stop DbError] r =>
  DbError ->
  Sem r a
resetConnection err = do
  atomicPut True
  resume_ DbConnection.reset
  stop err

connectWithReset ::
  Members [DbConnection Connection ! DbConnectionError, AtomicState Bool, Time t dt, Stop DbError, Embed IO] r =>
  Maybe TableStructure ->
  q ->
  Statement q o ->
  Sem r o
connectWithReset table q statement = do
  connection <- connectTable table
  traverseLeft resetConnection =<< runSession connection (Session.statement q statement)
{-# INLINE connectWithReset #-}

retrying ::
  TimeUnit u =>
  Members [DbConnection Connection ! DbConnectionError, AtomicState Bool, Time t d, Stop DbError, Embed IO] r =>
  Maybe TableStructure ->
  u ->
  q ->
  Statement q o ->
  Sem r o
retrying table interval q statement =
  traverseLeft recurseOnConnectionError =<< runStop (connectWithReset table q statement)
  where
    recurseOnConnectionError = \case
      DbError.Connection _ -> do
        resume_ DbConnection.reset
        Time.sleep interval
        retrying table interval q statement
      e ->
        stop e
{-# INLINE retrying #-}

interpretDatabaseState ::
  ∀ t dt r .
  Members [DbConnection Connection ! DbConnectionError, AtomicState Bool, Time t dt, Embed IO] r =>
  InterpreterFor (Database ! DbError) r
interpretDatabaseState =
  interpretResumableH \case
    Run table q statement ->
      pureT =<< connectWithReset table q statement
    Retrying table interval q statement ->
      pureT =<< retrying table interval q statement
{-# INLINE interpretDatabaseState #-}

interpretDatabase ::
  ∀ t dt r .
  Members [DbConnection Connection ! DbConnectionError, Time t dt, Embed IO] r =>
  InterpreterFor (Database ! DbError) r
interpretDatabase =
  interpretAtomic True . interpretDatabaseState . raiseUnder
{-# INLINE interpretDatabase #-}
