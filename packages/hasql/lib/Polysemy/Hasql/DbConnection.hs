module Polysemy.Hasql.DbConnection where

import Polysemy (interpretH)
import Control.Lens (_2)
import Hasql.Connection (Connection, Settings)
import qualified Hasql.Connection as Connection (acquire, release, settings)
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Resource (Resource, finally)

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

cachedUse ::
  Members [AtomicState (Int, Maybe Connection), Embed IO, Stop DbConnectionError] r =>
  ((Int, Connection) -> Sem r a) ->
  DbConfig ->
  Sem r a
cachedUse f config = do
  f =<< create =<< atomicGet
  where
    create (count, Nothing) = do
      conn <- connect config
      atomicPut (count + 1, Just conn)
      pure (count + 1, conn)
    create (count, Just conn) =
      pure (count, conn)

disconnect ::
  Members [AtomicState (Int, Maybe Connection), Stop DbConnectionError, Embed IO] r =>
  Maybe Connection ->
  Sem r ()
disconnect = \case
  Just connection -> do
    stopEither . first DbConnectionError.Release =<< tryAny (Connection.release connection)
    atomicModify' (_2 .~ Nothing)
  Nothing ->
    unit

useSingleton ::
  Members [Embed IO, Stop DbConnectionError] r =>
  (Connection -> Sem r a) ->
  DbConfig ->
  Sem r a
useSingleton f config =
  f =<< connect config

interpretDbConnectionSingleton ::
  Members [Embed IO, Stop DbConnectionError] r =>
  DbConfig ->
  InterpreterFor (DbConnection Connection) r
interpretDbConnectionSingleton config =
  interpretH \case
    DbConnection.Info ->
      pureT ("singleton", 0)
    DbConnection.Use f ->
      useSingleton (callT (f 0)) config
    DbConnection.Disconnect ->
      liftT unit
    DbConnection.Reset ->
      liftT unit

interpretDbConnectionCached ::
  ∀ r .
  Members [AtomicState (Int, Maybe Connection), Embed IO] r =>
  Text ->
  DbConfig ->
  InterpreterFor HasqlConnection r
interpretDbConnectionCached name config =
  interpretResumableH \case
    DbConnection.Info ->
      liftT ((name,) <$> atomicGets fst)
    DbConnection.Use f -> do
      cachedUse (callT (uncurry f)) config
    DbConnection.Disconnect ->
      liftT (disconnect . snd =<< atomicGet)
    DbConnection.Reset ->
      liftT (atomicModify' (_2 .~ Nothing))

withDisconnect ::
  Members [DbConnection c !! DbConnectionError, Resource] r =>
  Sem r a ->
  Sem r a
withDisconnect sem =
  finally sem (resume DbConnection.disconnect \ _ -> unit)

-- |Connects to a database and shares the connection among all consumers of the interpreter.
interpretDbConnection ::
  Members [Resource, Embed IO] r =>
  Text ->
  DbConfig ->
  InterpreterFor HasqlConnection r
interpretDbConnection name config sem =
  interpretAtomic (0, Nothing) (interpretDbConnectionCached name config (raiseUnder (withDisconnect sem)))
