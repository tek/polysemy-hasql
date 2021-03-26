module Polysemy.Hasql.DbConnection where

import Control.Concurrent (myThreadId, throwTo)
import Hasql.Connection (Connection, Settings)
import qualified Hasql.Connection as Connection (acquire, release, settings)
import Polysemy (interpretH)
import Polysemy.Db.Atomic (interpretAtomic)
import Polysemy.Db.Data.DbConfig (DbConfig(DbConfig))
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbHost (DbHost(DbHost))
import Polysemy.Db.Data.DbName (DbName(DbName))
import Polysemy.Db.Data.DbPassword (DbPassword(DbPassword))
import Polysemy.Db.Data.DbPort (DbPort(DbPort))
import Polysemy.Db.Data.DbUser (DbUser(DbUser))
import Polysemy.Error (fromExceptionSem)
import Polysemy.Internal.Tactics (liftT)
import Polysemy.Resource (Resource, bracket_, finally)

import qualified Polysemy.Hasql.Data.ConnectionState as ConnectionState
import Polysemy.Hasql.Data.ConnectionState (ConnectionState(ConnectionState))
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import Polysemy.Hasql.Data.DbConnection (DbConnection)
import Polysemy.Hasql.Database (HasqlConnection)

data KillCommand =
  KillCommand
  deriving (Show, Exception)

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

withThreadIdInState ::
  Members [AtomicState ConnectionState, Resource, Embed IO] r =>
  Sem r a ->
  Sem r a
withThreadIdInState =
  bracket_ (setTid . Just =<< embed myThreadId) (setTid Nothing)
  where
    setTid tid =
      atomicModify' (ConnectionState.activeCommand .~ tid)

catchingKill ::
  Members [Stop DbConnectionError, Final IO] r =>
  Sem r a ->
  Sem r a
catchingKill =
  stopOnError . mapError exception . fromExceptionSem . raiseUnder . raise
  where
    exception (_ :: KillCommand) =
      DbConnectionError.Query "command was interrupted by DbConnection.Kill"

connecting ::
  Members [AtomicState ConnectionState, Stop DbConnectionError, Resource, Embed IO] r =>
  DbConfig ->
  (Int -> Connection -> Sem r a) ->
  Sem r a
connecting config use =
  uncurry use =<< create =<< atomicGet
  where
    create (ConnectionState count Nothing tid) = do
      conn <- connect config
      atomicPut (ConnectionState (count + 1) (Just conn) tid)
      pure (count + 1, conn)
    create (ConnectionState count (Just conn) _) =
      pure (count, conn)

cachedUse ::
  Members [AtomicState ConnectionState, Stop DbConnectionError, Resource, Embed IO, Final IO] r =>
  (Int -> Connection -> Sem r a) ->
  DbConfig ->
  Sem r a
cachedUse f config = do
  connecting config \ count conn ->
    withThreadIdInState (catchingKill (f count conn))

-- TODO error when ConnectionState.activeCommand is `Just`
disconnect ::
  Members [AtomicState ConnectionState, Stop DbConnectionError, Embed IO] r =>
  ConnectionState ->
  Sem r ()
disconnect = \case
  ConnectionState _ (Just connection) _ -> do
    stopEither . first DbConnectionError.Release =<< tryAny (Connection.release connection)
    atomicModify' (ConnectionState.connection .~ Nothing)
  ConnectionState _ Nothing _ ->
    unit

kill ::
  Members [AtomicState ConnectionState, Stop DbConnectionError, Embed IO] r =>
  ConnectionState ->
  Sem r ()
kill = \case
  ConnectionState _ (Just _) (Just tid) -> do
    embed (throwTo tid KillCommand)
    disconnect  =<< atomicGet
  ConnectionState _ _ _ ->
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
    DbConnection.Kill ->
      liftT unit
    DbConnection.Reset ->
      liftT unit

-- TODO connection should probably use an `MVar`. If a connection is in use by `listen`, using it again will block
-- indefinitely.
interpretDbConnectionCached ::
  ∀ r .
  Members [AtomicState ConnectionState, Resource, Embed IO, Final IO] r =>
  Text ->
  DbConfig ->
  InterpreterFor HasqlConnection r
interpretDbConnectionCached name config =
  interpretResumableH \case
    DbConnection.Info ->
      liftT ((name,) <$> atomicGets ConnectionState._count)
    DbConnection.Use f -> do
      cachedUse (curry (callT (uncurry f))) config
    DbConnection.Disconnect ->
      liftT (disconnect =<< atomicGet)
    DbConnection.Kill -> do
      liftT (kill =<< atomicGet)
    DbConnection.Reset ->
      liftT (atomicModify' (ConnectionState.connection .~ Nothing))

withDisconnect ::
  Members [DbConnection c !! DbConnectionError, Resource] r =>
  Sem r a ->
  Sem r a
withDisconnect sem =
  finally sem (resume DbConnection.disconnect \ _ -> unit)

-- |Connects to a database and shares the connection among all consumers of the interpreter.
-- To fully run it:
--
-- >>> runM (resourceToIO (interpretDbConnection prog))
interpretDbConnection ::
  Members [Resource, Embed IO, Final IO] r =>
  -- |The arbitrary name of the connection, for inspection purposes
  Text ->
  -- |The connection configuration
  DbConfig ->
  InterpreterFor HasqlConnection r
interpretDbConnection name config sem =
  interpretAtomic (ConnectionState 0 Nothing Nothing) $
  interpretDbConnectionCached name config (raiseUnder (withDisconnect sem))
