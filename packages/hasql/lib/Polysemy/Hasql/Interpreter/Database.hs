module Polysemy.Hasql.Interpreter.Database where

import Conc (Lock, interpretAtomic, interpretLockReentrant, interpretResumableScopedWithH, lock)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Exon (exon)
import Hasql.Connection (Connection, withLibPQConnection)
import qualified Log
import Polysemy.Db.Data.DbConfig (DbConfig)
import Polysemy.Db.Data.DbConnectionError (DbConnectionError)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Time
import Time (NanoSeconds (NanoSeconds))

import qualified Polysemy.Hasql.Data.ConnectionState as ConnectionState
import Polysemy.Hasql.Data.ConnectionState (ConnectionState (ConnectionState), ConnectionsState (ConnectionsState))
import Polysemy.Hasql.Data.ConnectionTag (ConnectionTag (GlobalTag, SerialTag))
import Polysemy.Hasql.Data.InitDb (InitDb (InitDb), hoistInitDb)
import Polysemy.Hasql.Effect.Database (
  ConnectionSource (Global, Supplied, Unique),
  Database (Release, ResetInit, Retry, Session, Tag, Use, WithInit),
  withDatabaseGlobal,
  )
import qualified Polysemy.Hasql.Effect.DbConnectionPool as DbConnectionPool
import Polysemy.Hasql.Effect.DbConnectionPool (DbConnectionPool)
import Polysemy.Hasql.Interpreter.DbConnectionPool (interpretDbConnectionPool)
import Polysemy.Hasql.Session (runSession)

genTag ::
  Member (AtomicState ConnectionsState) r =>
  Sem r ConnectionTag
genTag =
  SerialTag <$> atomicState' \ ConnectionsState {..} ->
    let new = counter + 1
    in (ConnectionsState {counter = new, ..}, new)

tagForSource ::
  Member (AtomicState ConnectionsState) r =>
  ConnectionSource ->
  Sem r (Either Connection ConnectionTag)
tagForSource = \case
  Global -> pure (Right GlobalTag)
  Unique t -> Right <$> fromMaybeA genTag t
  Supplied _ c -> pure (Left c)

needsInit ::
  Member (AtomicState ConnectionsState) r =>
  InitDb m ->
  Int ->
  Sem r Bool
needsInit (InitDb clientId once _) count =
  atomicView (#clientInits . at clientId) <&> \case
    Just lastConnection ->
      not once && lastConnection < count
    Nothing ->
      True

runInit ::
  Members [AtomicState ConnectionsState, Log, Embed IO] r =>
  InitDb (Sem r) ->
  Int ->
  Connection ->
  Sem r ()
runInit (InitDb clientId _ initDb) count connection = do
  Log.trace [exon|Running init for '##{clientId}'|]
  initDb connection
  atomicModify' (#clientInits . at clientId ?~ count)

acquireConnection ::
  Members [DbConnectionPool !! DbConnectionError, AtomicState ConnectionState, Stop DbError, Lock] r =>
  ConnectionTag ->
  Sem r (Int, Connection)
acquireConnection ctag =
  lock do
    atomicGet >>= \case
      ConnectionState count Nothing tids -> do
        conn <- resumeHoist DbError.Connection (DbConnectionPool.acquire ctag)
        atomicPut (ConnectionState (count + 1) (Just conn) tids)
        pure (count + 1, conn)
      ConnectionState count (Just conn) _ ->
        pure (count, conn)

releaseConnection ::
  Members [DbConnectionPool !! DbConnectionError, AtomicState ConnectionState, Log] r =>
  ConnectionTag ->
  Sem r ()
releaseConnection ctag = do
  atomicModify' (#connection .~ Nothing)
  DbConnectionPool.release ctag !! \ e ->
    Log.error [exon|Releasing connection failed: #{show e}|]

-- | After a computation failed, the Postgres connection needs to be health-checked.
-- If the status is 'LibPQ.ConnectionBad', remove the connection from the state and release it, causing the next call to
-- 'acquireConnection' to request a new one.
--
-- It is conceivable for the connection to be stuck in a startup phase like 'LibPQ.ConnectionSSLStartup', but since
-- hasql only uses a connection that is fully established, it shouldn't happen.
--
-- TODO Can't hurt to investigate this anyway.
releaseBadConnection ::
  Members [DbConnectionPool !! DbConnectionError, AtomicState ConnectionState, Stop DbError, Log, Lock, Embed IO] r =>
  ConnectionTag ->
  Connection ->
  Sem r ()
releaseBadConnection ctag conn =
  tryIOError (withLibPQConnection conn LibPQ.status) >>= \case
    Right LibPQ.ConnectionBad -> do
      Log.debug [exon|Releasing bad connection ##{ctag}|]
      releaseConnection ctag
    Left err ->
      Log.error [exon|Releasing bad connection failed: #{err}|]
    _ ->
      unit

bracketConnection ::
  Member Resource r =>
  Members [DbConnectionPool !! DbConnectionError, AtomicState ConnectionState, Stop DbError, Log, Lock, Embed IO] r =>
  ConnectionTag ->
  (Int -> Connection -> Sem r a) ->
  Sem r a
bracketConnection ctag use =
  resumeHoist DbError.Connection $ DbConnectionPool.use ctag $
  bracketOnError (acquireConnection ctag) onError (raise . uncurry use)
  where
    onError (_, conn) = releaseBadConnection ctag conn

withInit ::
  Members [AtomicState ConnectionsState, Stop DbError, Log, Embed IO] r =>
  Int ->
  Connection ->
  InitDb (Sem r) ->
  Sem r a ->
  Sem r a
withInit count connection initDb main = do
  whenM (needsInit initDb count) do
    runInit initDb count connection
  main

withInitManaged ::
  Members [AtomicState ConnectionState, AtomicState ConnectionsState, DbConnectionPool !! DbConnectionError] r =>
  Members [Stop DbError, Lock, Resource, Log, Embed IO] r =>
  ConnectionTag ->
  InitDb (Sem r) ->
  (Connection -> Sem r a) ->
  Sem r a
withInitManaged ctag initDb use = do
  bracketConnection ctag \ count connection -> do
    Log.trace [exon|Client '##{clientTag}' uses database connection '##{ctag}'|]
    withInit count connection initDb (use connection)
  where
    clientTag = initDb ^. #tag

retrying ::
  Members [DbConnectionPool !! DbConnectionError, Time t d, Stop DbError, Resource, Embed IO] r =>
  (Int, NanoSeconds) ->
  Sem (Stop DbError : r) a ->
  Sem r a
retrying (total, interval) action =
  spin total
  where
    spin 0 = subsume action
    spin count =
      runStop action >>= leftA \case
        DbError.Connection _ -> do
          Time.sleep interval
          spin (count - 1)
        e ->
          stop e

managedConnection ::
  ∀ r m t d a .
  Members [AtomicState ConnectionsState, AtomicState ConnectionState, DbConnectionPool !! DbConnectionError] r =>
  Members [Time t d, Resource, Lock, Log, Embed IO] r =>
  ConnectionTag ->
  InitDb (Sem r) ->
  Maybe (Int, NanoSeconds) ->
  Database m a ->
  Tactical (Database !! DbError) m (Stop DbError : r) a
managedConnection ctag initDb retryMay = \case
  WithInit (InitDb t o new) ma -> do
    s <- getInitialStateT
    newT <- bindT new
    let new' c = void (interpretResumableH (managedConnection ctag def Nothing) (newT (c <$ s)))
    raise . interpretResumableH (managedConnection ctag (InitDb t o new') retryMay) =<< runT ma
  Session ma -> do
    result <- retrying retry $ withInitManaged ctag (hoistInitDb (insertAt @0) initDb) \ connection ->
      runSession connection ma
    pureT result
  Use use ->
    withInitManaged ctag (hoistInitDb (insertAt @0) initDb) \ connection ->
      runTSimple (use connection)
  Release ->
    pureT =<< releaseConnection ctag
  Retry interval count ma -> do
    let r = Just (fromMaybe 0 count, Time.convert interval)
    raise . interpretResumableH (managedConnection ctag (hoistInitDb raise initDb) r) =<< runT ma
  Tag ->
    pureT ctag
  ResetInit ->
    pureT =<< atomicModify' @ConnectionsState (#clientInits .~ mempty)
  where
    retry = fromMaybe (0, NanoSeconds 0) retryMay

unmanagedConnection ::
  ∀ r m a .
  Members [AtomicState ConnectionsState, AtomicState ConnectionState, DbConnectionPool !! DbConnectionError] r =>
  Members [Resource, Lock, Log, Embed IO, Final IO] r =>
  Connection ->
  InitDb (Sem r) ->
  Database m a ->
  Tactical (Database !! DbError) m (Stop DbError : r) a
unmanagedConnection connection initDb = \case
  WithInit (InitDb t o new) ma -> do
    s <- getInitialStateT
    newT <- bindT new
    let new' c = void (interpretResumableH (unmanagedConnection connection def) (newT (c <$ s)))
    raise . interpretResumableH (unmanagedConnection connection (InitDb t o new')) =<< runT ma
  Session ma ->
    withInit 0 connection (hoistInitDb (insertAt @0) initDb) do
      pureT =<< runSession connection ma
  Use use ->
    withInit 0 connection (hoistInitDb (insertAt @0) initDb) do
      runTSimple (use connection)
  Release ->
    unitT
  -- TODO
  Retry _ _ ma ->
    runTSimple ma
  Tag ->
    pureT "unmanaged"
  -- TODO
  ResetInit ->
    unitT

handleDatabase ::
  ∀ r m a t d .
  Members [AtomicState ConnectionsState, AtomicState ConnectionState, DbConnectionPool !! DbConnectionError] r =>
  Members [Time t d, Resource, Lock, Log, Embed IO, Final IO] r =>
  Either Connection ConnectionTag ->
  Database m a ->
  Tactical (Database !! DbError) m (Stop DbError : r) a
handleDatabase = \case
  Right t -> managedConnection t def Nothing
  Left c -> unmanagedConnection c def

type DatabaseScope =
  [
    AtomicState ConnectionState,
    Lock
  ]

databaseScope ::
  Members [DbConnectionPool !! DbConnectionError, AtomicState ConnectionsState, Resource, Mask, Race, Embed IO] r =>
  (Either Connection ConnectionTag -> Sem (DatabaseScope ++ r) a) ->
  ConnectionSource ->
  Sem r a
databaseScope use source =
  interpretLockReentrant $ interpretAtomic def do
    ctag <- tagForSource source
    finally (use ctag) (traverse_ (resume_ . DbConnectionPool.free) ctag)

interpretDatabases ::
  ∀ t d r .
  Members [DbConnectionPool !! DbConnectionError, AtomicState ConnectionsState] r =>
  Members [Time t d, Log, Resource, Mask, Race, Embed IO, Final IO] r =>
  InterpreterFor (Scoped ConnectionSource (Database !! DbError)) r
interpretDatabases =
  interpretResumableScopedWithH @DatabaseScope (flip databaseScope) handleDatabase

interpretDatabase ::
  ∀ t d r .
  Members [DbConnectionPool !! DbConnectionError, Time t d, Resource, Log, Mask, Race, Embed IO, Final IO] r =>
  InterpretersFor [Database !! DbError, Scoped ConnectionSource (Database !! DbError)] r
interpretDatabase =
  interpretAtomic (ConnectionsState 0 mempty) .
  interpretDatabases .
  raiseUnder .
  withDatabaseGlobal

type HasqlStack =
  [
    Database !! DbError,
    Scoped ConnectionSource (Database !! DbError),
    DbConnectionPool !! DbConnectionError
  ]

interpretHasql ::
  Members [Time t d, Log, Mask, Resource, Race, Embed IO, Final IO] r =>
  DbConfig ->
  Maybe Int ->
  Maybe Int ->
  InterpretersFor HasqlStack r
interpretHasql dbConfig maxActive maxAvailable =
  interpretDbConnectionPool dbConfig maxActive maxAvailable .
  interpretDatabase
