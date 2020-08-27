module Polysemy.Db.Database where

import Hasql.Connection (Connection)
import qualified Hasql.Session as Session (statement)

import Polysemy.Db.Data.Database (Database(..))
import qualified Polysemy.Db.Data.DbConnection as DbConnection
import Polysemy.Db.Data.DbConnection (DbConnection)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.TableStructure (TableStructure)
import Polysemy.Db.Session (runSession)
import Polysemy.Db.Table (initTable)
import Polysemy.Db.Table.TableStructure (GenTableStructure(genTableStructure))

initOnReconnect ::
  Members '[State Bool, Error DbError, Embed IO] r =>
  Connection ->
  TableStructure ->
  Sem r ()
initOnReconnect connection table =
  whenM get $ do
    initTable connection table
    put False

connect ::
  Members '[DbConnection, State Bool, Error DbError, Embed IO] r =>
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
  Member (DbConnection) r =>
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
  Members [DbConnection, State Bool, Embed IO] r =>
  TableStructure ->
  InterpreterFor (Database d DbError) r
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
  Members [DbConnection, Embed IO] r =>
  TableStructure ->
  InterpreterFor (Database d DbError) r
interpretDatabase table =
  evalState True . interpretDatabaseState table . raiseUnder

interpretDatabaseGen ::
  ∀ d rep r .
  GenTableStructure d rep =>
  Members [Embed IO, DbConnection] r =>
  InterpreterFor (Database d DbError) r
interpretDatabaseGen =
  interpretDatabase (genTableStructure @d @rep)
