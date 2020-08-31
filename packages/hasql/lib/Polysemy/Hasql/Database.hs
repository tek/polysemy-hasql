module Polysemy.Hasql.Database where

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session (statement)

import Polysemy.Hasql.Data.Database (Database(..))
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.TableStructure (TableStructure)
import Polysemy.Hasql.Session (runSession)
import Polysemy.Hasql.Table (initTable)
import Polysemy.Hasql.Table.TableStructure (GenTableStructure(genTableStructure))

initOnReconnect ::
  Members [State Bool, Error DbError, Embed IO] r =>
  Connection ->
  TableStructure ->
  Sem r ()
initOnReconnect connection table =
  whenM get $ do
    initTable connection table
    put False

connect ::
  Members [DbConnection Connection, State Bool, Error DbError, Embed IO] r =>
  TableStructure ->
  Sem r Connection
connect table =
  check =<< DbConnection.connect
  where
    check = \case
      Right connection ->
        initOnReconnect connection table *> pure connection
      Left err ->
        put True *> throw err

resetConnection ::
  Member (DbConnection c) r =>
  DbError ->
  Sem r (Either DbError a)
resetConnection = \case
  DbError.Connection err -> do
    DbConnection.reset
    pure (Left (DbError.Connection err))
  err ->
    pure (Left err)

interpretDatabaseState ::
  ∀ d r .
  Members [DbConnection Connection, State Bool, Embed IO] r =>
  TableStructure ->
  InterpreterFor (Database DbError d) r
interpretDatabaseState table =
  interpret \case
    Run q statement ->
      runError (connect table) >>= \case
        Right connection ->
          either resetConnection (pure . Right) =<< runSession connection (Session.statement q statement)
        Left err ->
          pure (Left err)

interpretDatabase ::
  ∀ d r .
  Members [DbConnection Connection, Embed IO] r =>
  TableStructure ->
  InterpreterFor (Database DbError d) r
interpretDatabase table =
  evalState True . interpretDatabaseState table . raiseUnder

interpretDatabaseGen ::
  ∀ d rep r .
  GenTableStructure d rep =>
  Members [Embed IO, DbConnection Connection] r =>
  InterpreterFor (Database DbError d) r
interpretDatabaseGen =
  interpretDatabase (genTableStructure @d @rep)
