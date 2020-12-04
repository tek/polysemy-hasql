module Polysemy.Hasql.DbConnection where

import Hasql.Connection (Connection, Settings)
import qualified Hasql.Connection as Connection (acquire, release, settings)
import Polysemy (interpretH)
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Resource (Resource, finally)
import Polysemy.Resume (Stop, stopEither, type (!))

import Polysemy.Db.Atomic (interpretAtomic)
import Polysemy.Db.Data.DbConfig (DbConfig(DbConfig))
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbHost (DbHost(DbHost))
import Polysemy.Db.Data.DbName (DbName(DbName))
import Polysemy.Db.Data.DbPassword (DbPassword(DbPassword))
import Polysemy.Db.Data.DbPort (DbPort(DbPort))
import Polysemy.Db.Data.DbUser (DbUser(DbUser))
import Polysemy.Hasql (HasqlConnection)
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Resume (interpretResumableH, resume)

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
  Members [Embed IO, Stop DbConnectionError] r =>
  DbConfig ->
  Sem r Connection
connect (DbConfig host port name user password) =
  stopEither . (first dbError) =<< embed (Connection.acquire (connectionSettings host port name user password))
  where
    dbError err =
      DbConnectionError.Acquire (maybe "unspecified error" decodeUtf8 err)

cachedConnectWithInit ::
  Members [AtomicState (Maybe Connection), Embed IO, Stop DbConnectionError] r =>
  (Connection -> Sem r b) ->
  DbConfig ->
  Sem r Connection
cachedConnectWithInit init' config =
  maybe create pure =<< atomicGet
  where
    create = do
      d <- connect config
      atomicPut (Just d)
      init' d
      pure d

disconnect ::
  Members [AtomicState (Maybe Connection), Stop DbConnectionError, Embed IO] r =>
  Maybe Connection ->
  Sem r ()
disconnect = \case
  Just connection -> do
    stopEither . first DbConnectionError.Release =<< tryAny (Connection.release connection)
    atomicPut Nothing
  Nothing ->
    unit

connectSingleton ::
  Members [Embed IO, Stop DbConnectionError] r =>
  (Connection -> Sem r b) ->
  DbConfig ->
  Sem r Connection
connectSingleton init' config = do
  connection <- connect config
  connection <$ init' connection

interpretDbConnectionSingleton ::
  Members [Embed IO, Stop DbConnectionError] r =>
  DbConfig ->
  InterpreterFor (DbConnection Connection) r
interpretDbConnectionSingleton config =
  interpretH \case
    DbConnection.ConnectWithInit init' -> do
      pureT =<< connectSingleton (callT init') config
    DbConnection.Disconnect ->
      liftT unit
    DbConnection.Reset ->
      liftT unit

interpretDbConnectionCached ::
  ∀ r .
  Members [AtomicState (Maybe Connection), Embed IO] r =>
  DbConfig ->
  InterpreterFor HasqlConnection r
interpretDbConnectionCached config =
  interpretResumableH \case
    DbConnection.ConnectWithInit init' ->
      pureT =<< cachedConnectWithInit (callT init') config
    DbConnection.Disconnect ->
      liftT (disconnect =<< atomicGet)
    DbConnection.Reset ->
      liftT (atomicPut Nothing)

withDisconnect ::
  Members [DbConnection c ! DbConnectionError, Resource] r =>
  Sem r a ->
  Sem r a
withDisconnect sem =
  finally sem (resume DbConnection.disconnect \ _ -> unit)

-- |Connects to a database and shares the connection among all consumers of the interpreter.
interpretDbConnection ::
  Members [Resource, Embed IO] r =>
  DbConfig ->
  InterpreterFor HasqlConnection r
interpretDbConnection config sem =
  interpretAtomic Nothing (interpretDbConnectionCached config (raiseUnder (withDisconnect sem)))
