module Polysemy.Db.DbConnection where

import Hasql.Connection (Connection, Settings)
import qualified Hasql.Connection as Connection (acquire, settings)

import Polysemy.Db.Atomic (interpretAtomic)
import Polysemy.Db.Data.DbConfig (DbConfig(DbConfig))
import qualified Polysemy.Db.Data.DbConnection as DbConnection
import Polysemy.Db.Data.DbConnection (DbConnection)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.DbError as DbError (DbError(Connection))
import Polysemy.Db.Data.DbHost (DbHost(DbHost))
import Polysemy.Db.Data.DbName (DbName(DbName))
import Polysemy.Db.Data.DbPassword (DbPassword(DbPassword))
import Polysemy.Db.Data.DbPort (DbPort(DbPort))
import Polysemy.Db.Data.DbUser (DbUser(DbUser))

connectionSettings ::
  DbHost ->
  DbPort ->
  DbName ->
  DbUser ->
  DbPassword ->
  Settings
connectionSettings (DbHost host) (DbPort port) (DbName dbName) (DbUser user) (DbPassword password) =
  Connection.settings (encodeUtf8 host) (fromIntegral port) (encodeUtf8 user) (encodeUtf8 password) (encodeUtf8 dbName)

connect ::
  Members [Embed IO, Error DbError] r =>
  DbConfig ->
  Sem r Connection
connect (DbConfig host port name user password) = do
  hoistEither dbError =<< embed (Connection.acquire (connectionSettings host port name user password))
  where
    dbError err =
      DbError.Connection (maybe "unspecified error" decodeUtf8 err)

cachedConnect ::
  Members [AtomicState (Maybe Connection), Embed IO] r =>
  DbConfig ->
  Sem r (Either DbError Connection)
cachedConnect config =
  runError . maybe create pure =<< atomicGet
  where
    create = do
      d <- connect config
      atomicPut (Just d)
      pure d

interpretDbConnectionSingleton ::
  Member (Embed IO) r =>
  DbConfig ->
  InterpreterFor DbConnection r
interpretDbConnectionSingleton config =
  interpret \case
    DbConnection.Connect ->
      runError (connect config)
    DbConnection.Reset ->
      unit

interpretDbConnectionCached ::
  Members [AtomicState (Maybe Connection), Embed IO] r =>
  DbConfig ->
  InterpreterFor DbConnection r
interpretDbConnectionCached config =
  interpret \case
    DbConnection.Connect ->
      cachedConnect config
    DbConnection.Reset ->
      atomicPut Nothing

-- |Connects to a database and shares the connection among all consumers of the interpreter.
-- The @'Error' 'DbError'@ effect is exposed because the connection is expected to be crucial and allocated at startup.
interpretDbConnection ::
  Member (Embed IO) r =>
  DbConfig ->
  InterpreterFor DbConnection r
interpretDbConnection config =
  interpretAtomic Nothing .
  interpretDbConnectionCached config .
  raiseUnder
