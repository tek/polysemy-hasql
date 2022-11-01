module Polysemy.Hasql.Interpreter.DbConnectionPool where

import Conc (interpretAtomic)
import Control.Concurrent (ThreadId, myThreadId, throwTo)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Data.Sequence as Seq
import Data.Sequence (Seq ((:<|)), (<|))
import Exon (exon)
import qualified Hasql.Connection as Connection
import Hasql.Connection (Connection)
import Lens.Micro.Extras (view)
import qualified Log
import Polysemy.Db.Data.DbConfig (DbConfig (DbConfig))
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import Polysemy.Db.Data.DbHost (DbHost (DbHost))
import Polysemy.Db.Data.DbName (DbName (DbName))
import Polysemy.Db.Data.DbPassword (DbPassword (DbPassword))
import Polysemy.Db.Data.DbUser (DbUser (DbUser))
import qualified Text.Show as Show

import Polysemy.Hasql.Data.ConnectionTag (ConnectionTag)
import Polysemy.Hasql.Effect.DbConnectionPool (DbConnectionPool (Acquire, Free, Kill, Release, UnsafeGet, Use))

data KillCommand =
  KillCommand
  deriving stock (Show)
  deriving anyclass (Exception)

newtype PoolConn =
  PoolConn { unPoolConn :: Connection }
  deriving stock (Generic)

instance Show PoolConn where
  show _ = "PoolConn"

data ConnectionClients =
  ConnectionClients {
    connection :: PoolConn,
    clients :: Map ThreadId Int
  }
  deriving stock (Show, Generic)

data Pools =
  Pools {
    maxActive :: Maybe Int,
    maxAvailable :: Maybe Int,
    active :: Map ConnectionTag ConnectionClients,
    available :: Seq PoolConn
  }
  deriving stock (Show, Generic)

connectionSettings ::
  DbConfig ->
  Connection.Settings
connectionSettings (DbConfig (DbHost host) port (DbName dbName) (DbUser user) (DbPassword password)) =
  Connection.settings (encodeUtf8 host) (fromIntegral port) (encodeUtf8 user) (encodeUtf8 password) (encodeUtf8 dbName)

dbError :: Maybe ByteString -> DbConnectionError
dbError err =
  DbConnectionError.Acquire (maybe "unspecified error" decodeUtf8 err)

withActive ::
  Member (AtomicState Pools) r =>
  (Int -> Map ConnectionTag ConnectionClients -> Sem r a) ->
  Sem r (Maybe a)
withActive f =
  atomicGet >>= \case
    Pools {maxActive = Just ma, ..} ->
      Just <$> f ma active
    _ ->
      pure Nothing

acquireNative ::
  Members [Stop DbConnectionError, Embed IO] r =>
  DbConfig ->
  Sem r Connection
acquireNative dbConfig = do
  conn <- stopTryIOError DbConnectionError.Acquire (Connection.acquire (connectionSettings dbConfig))
  stopEither (first dbError conn)

acquire ::
  Members [AtomicState Pools, Stop DbConnectionError, Embed IO] r =>
  DbConfig ->
  ConnectionTag ->
  Sem r Connection
acquire dbConfig ctag = do
  void $ withActive \ m act ->
    when (Map.size act >= m) (stop (DbConnectionError.Limit [exon|Too many active connections: #{show m}|]))
  conn <- acquireNative dbConfig
  conn <$ atomicModify' (#active . at ctag ?~ ConnectionClients (PoolConn conn) mempty)

reuseOrAcquire ::
  Members [AtomicState Pools, Stop DbConnectionError, Log, Embed IO] r =>
  DbConfig ->
  ConnectionTag ->
  Sem r Connection
reuseOrAcquire dbConfig ctag = do
  reuse <- atomicState' \ pools@Pools {..} ->
    case active !? ctag of
      Just (ConnectionClients (PoolConn conn) _) ->
        (pools, Just conn)
      Nothing ->
        case available of
          PoolConn conn :<| rest ->
            (pools {available = rest}, Just conn)
          _ ->
            (pools, Nothing)
  when (isJust reuse) do
    Log.trace [exon|Reusing connection for '##{ctag}'|]
  fromMaybeA (acquire dbConfig ctag) reuse

releaseNative ::
  Members [Stop DbConnectionError, Embed IO] r =>
  Connection ->
  Sem r ()
releaseNative connection = do
  stopTryIOError DbConnectionError.Release (Connection.release connection)

release ::
  Members [AtomicState Pools, Stop DbConnectionError, Embed IO] r =>
  ConnectionTag ->
  Sem r ()
release ctag = do
  conn <- atomicState' \ Pools {..} -> (Pools {active = Map.delete ctag active, ..}, active !? ctag)
  traverse_ (releaseNative . coerce . connection) conn

-- | Remove the connection used by @ctag@ from the active pool if it exists.
-- Store it for reuse if @maxAvailable@ is @Nothing@ or larger than the currently stored number, otherwise return the
-- connection for release.
removeActive :: ConnectionTag -> Pools -> (Pools, Maybe Connection)
removeActive ctag Pools {..} =
  (Pools {active = newActive, available = newAvailable, ..}, coerce toRelease)
  where
    -- Chooses the functor @(Maybe Connection, -)@ for @alterF@, thereby returning the potentially existing element.
    -- The @Nothing@ causes the element to be deleted if it exists.
    (conn, newActive) = Map.alterF (,Nothing) ctag active

    (toRelease, newAvailable) = case conn of
      Just (ConnectionClients c _)
        | keep -> (Nothing, c <| available)
        | otherwise -> (Just c, available)
      Nothing -> (Nothing, available)

    keep = case maxAvailable of
      Nothing -> True
      Just m -> Seq.length available < m

catchingKill ::
  Members [Stop DbConnectionError, Final IO] r =>
  Sem r a ->
  Sem r a
catchingKill =
  stopOnError . mapError exception . fromExceptionSem . raiseUnder . raise
  where
    exception KillCommand =
      DbConnectionError.Query "command was interrupted by DbConnectionPool.Kill"

-- Incrementing the tid must not be masked or it might not be cleaned up.
withRegisteredClient ::
  Members [AtomicState Pools, Stop DbConnectionError, Resource, Embed IO, Final IO] r =>
  ConnectionTag ->
  Sem r a ->
  Sem r a
withRegisteredClient ctag main = do
  tid <- embed myThreadId
  finally
    do
      catchingKill do
        change tid increment
        main
    do
      change tid decrement
  where
    change tid f =
      atomicModify' (#active . at ctag %~ fmap (#clients %~ Map.alter f tid))
    increment = \case
      Just n -> Just (n + 1)
      Nothing -> Just 1
    decrement = \case
      Just 1 -> Nothing
      Just n -> Just (n - 1)
      Nothing -> Nothing

releaseAll ::
  Members [AtomicState Pools, Log, Resource, Embed IO, Final IO] r =>
  Sem r ()
releaseAll =
  atomicGet >>= \ Pools {active, available} -> do
    for_ (Map.elems active) \ (ConnectionClients conn _) ->
      releaseOrLog conn
    traverse_ releaseOrLog available
  where
    releaseOrLog (PoolConn conn) =
      runStop (releaseNative conn) >>= leftA \ e ->
        Log.error [exon|Releasing connection failed: #{show e}|]

handleDbConnectionPool ::
  Members [AtomicState Pools, Stop DbConnectionError, Log, Resource, Embed IO, Final IO] r =>
  DbConfig ->
  DbConnectionPool m a ->
  Tactical e m r a
handleDbConnectionPool dbConfig = \case
  Acquire ctag -> do
    Log.trace [exon|Acquiring connection '##{ctag}'|]
    pureT =<< reuseOrAcquire dbConfig ctag
  Free ctag -> do
    traverse_ releaseNative =<< atomicState' (removeActive ctag)
    unitT
  Release ctag -> do
    Log.trace [exon|Releasing connection '##{ctag}'|]
    pureT =<< release ctag
  Use ctag ma ->
    withRegisteredClient ctag (runTSimple ma)
  Kill ctag -> do
    cur <- embed myThreadId
    atomicGets (view (#active . at ctag)) >>= traverse_ \ (ConnectionClients _ clients) -> do
      for_ (Map.keys clients) \ c -> do
        unless (cur == c) (embed (throwTo c KillCommand))
    pureT =<< release ctag
  UnsafeGet ctag ->
    pureT . fmap (coerce . connection) =<< atomicGets (view (#active . at ctag))

interpretDbConnectionPool ::
  Members [Log, Resource, Embed IO, Final IO] r =>
  DbConfig ->
  Maybe Int ->
  Maybe Int ->
  InterpreterFor (DbConnectionPool !! DbConnectionError) r
interpretDbConnectionPool dbConfig maxActive maxAvailable =
  interpretAtomic (Pools maxActive maxAvailable mempty mempty) .
  flip finally releaseAll .
  interpretResumableH (handleDbConnectionPool dbConfig) .
  raiseUnder

handleDbConnectionPoolSingle ::
  Members [AtomicState (Maybe Connection), Stop DbConnectionError, Embed IO] r =>
  DbConfig ->
  DbConnectionPool m a ->
  Tactical e m r a
handleDbConnectionPoolSingle dbConfig = \case
  Acquire _ -> do
    let
      acquireSingle = do
        c <- acquireNative dbConfig
        c <$ atomicPut (Just c)
    pureT =<< fromMaybeA acquireSingle =<< atomicGet
  Free _ ->
    unitT
  -- TODO this should be called from outside of the DbConnection scope interpreter only
  Release _ -> do
    traverse_ releaseNative =<< atomicGet
    unitT
  -- TODO maybe not very useful but possible
  Use _ ma ->
    runTSimple ma
  Kill _ ->
    unitT
  UnsafeGet _ ->
    pureT Nothing

interpretDbConnectionPoolSingle ::
  Member (Embed IO) r =>
  DbConfig ->
  InterpreterFor (DbConnectionPool !! DbConnectionError) r
interpretDbConnectionPoolSingle dbConfig =
  interpretAtomic Nothing .
  interpretResumableH (handleDbConnectionPoolSingle dbConfig) .
  raiseUnder
