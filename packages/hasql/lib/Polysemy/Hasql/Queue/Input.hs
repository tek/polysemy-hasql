module Polysemy.Hasql.Queue.Input where

import Conc (
  ClockSkewConfig,
  Monitor,
  Restart,
  RestartingMonitor,
  interpretAtomic,
  interpretMonitorRestart,
  monitor,
  monitorClockSkew,
  restart,
  )
import Control.Concurrent (threadWaitRead)
import qualified Control.Concurrent.Async as Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Control.Exception (IOException)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Exon (exon)
import Hasql.Connection (Connection, withLibPQConnection)
import qualified Log
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Final (withWeavingToFinal)
import Polysemy.Input (Input (Input))
import Prelude hiding (Queue, listen)
import Sqel (Uuid, sql)
import qualified Sqel.Data.Uid as Uid
import Sqel.SOP.Constraint (symbolText)
import qualified Time as Time
import Torsor (Torsor)

import Polysemy.Hasql.Data.ConnectionTag (ConnectionTag (NamedTag))
import Polysemy.Hasql.Data.InitDb (InitDb (InitDb))
import qualified Polysemy.Hasql.Database as Database (retryingSqlDef)
import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database, Databases, withDatabaseUnique)
import Polysemy.Hasql.Queue.Data.Queue (Queue, QueueName (QueueName))
import Polysemy.Hasql.Queue.Data.Queued (Queued)
import qualified Polysemy.Hasql.Queue.Data.Queued as Queued (Queued (..))

-- | Try to fetch a notification, and if there is none, wait on the connection's file descriptor until some data is
-- received.
-- This connection will be fully blocked when waiting, so it must not be shared with other parts of the application.
--
-- TODO Could it be possible to share a connection among all queues, only for waiting?
tryDequeue ::
  Members [Monitor Restart, Reader QueueName, Log, Embed IO] r =>
  LibPQ.Connection ->
  Sem r (Either Text (Maybe UUID))
tryDequeue connection = do
  QueueName name <- ask
  let status msg = Log.trace [exon|#{msg} on connection for '#{name}'|]
  status "Trying dequeue"
  embed (LibPQ.notifies connection) >>= \case
    Just (LibPQ.Notify _ _ payload) -> do
      status "Received notify"
      case UUID.fromASCIIBytes payload of
        Just d ->
          pure (Right (Just d))
        Nothing ->
          pure (Left [exon|invalid UUID payload: #{decodeUtf8 payload}|])
    Nothing -> do
      status "No notify"
      embed (LibPQ.socket connection) >>= \case
        Just fd -> do
          status "Waiting for activity"
          monitor (embed (threadWaitRead fd))
          status "Activity received"
          Right Nothing <$ embed (LibPQ.consumeInput connection)
        Nothing ->
          pure (Left "couldn't connect with LibPQ.socket")

listen ::
  Members [Database, Reader QueueName, Log, Embed IO] r =>
  Sem r ()
listen = do
  QueueName name <- ask
  Log.debug [exon|executing `listen` for queue ##{name}|]
  Database.retryingSqlDef [sql|listen "##{name}"|]

unlisten ::
  ∀ e r .
  Members [Database !! e, Reader QueueName, Log] r =>
  Sem r ()
unlisten = do
  QueueName name <- ask
  Log.debug [exon|executing `unlisten` for queue `##{name}`|]
  resume_ (Database.retryingSqlDef [sql|unlisten "##{name}"|])

processMessages ::
  Ord t =>
  NonEmpty (Uuid (Queued t d)) ->
  NonEmpty d
processMessages =
  fmap (\ u -> u.payload.queue_payload) .
  NonEmpty.sortWith \ u -> u.payload.queue_created

initQueue ::
  ∀ e d t r .
  Ord t =>
  Members [Store UUID (Queued t d) !! e, Reader QueueName, Database, Log, Embed IO] r =>
  (d -> Sem r ()) ->
  Sem r ()
initQueue write = do
  QueueName name <- ask
  Log.trace [exon|Initializing queue '#{name}'|]
  waiting <- resumeAs Nothing (nonEmpty <$> Store.deleteAll)
  traverse_ (traverse_ write . processMessages) waiting
  listen

withPqConn ::
  Member (Final IO) r =>
  Connection ->
  (LibPQ.Connection -> Sem r a) ->
  Sem r (Either Text a)
withPqConn connection use =
  errorToIOFinal $ fromExceptionSemVia @IOException show $ withWeavingToFinal \ s lower _ -> do
    withLibPQConnection connection \ c -> lower (raise (use c) <$ s)

-- TODO check that DbConnectionError is right here
dequeueAndProcess ::
  ∀ d t dt r .
  Ord t =>
  Members [Monitor Restart, Reader QueueName, Final IO] r =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Stop DbError, Log, Embed IO] r =>
  TBMQueue d ->
  Connection ->
  Sem r ()
dequeueAndProcess queue connection = do
  result <- join <$> withPqConn connection tryDequeue
  void $ runMaybeT do
    id' <- MaybeT (stopEitherWith (DbError.Connection . DbConnectionError.Acquire) result)
    messages <- MaybeT (restop (Store.delete id'))
    liftIO (traverse_ (atomically . writeTBMQueue queue) (processMessages (pure messages)))

dequeue ::
  ∀ d t dt r .
  Ord t =>
  Members [Monitor Restart, Reader QueueName, Final IO] r =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Stop DbError, Time t dt, Log, Embed IO] r =>
  TBMQueue d ->
  Sem r ()
dequeue queue = do
  QueueName name <- ask
  let
    initDb = InitDb [exon|dequeue-##{name}|] False \ _ -> initQueue (embed . atomically . writeTBMQueue queue)
  restop @_ @Database do
    Database.withInit initDb (Database.use (dequeueAndProcess queue))

dequeueLoop ::
  ∀ d t dt u r .
  Ord t =>
  TimeUnit u =>
  Members [Monitor Restart, Reader QueueName] r =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Resource, Embed IO, Final IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  TBMQueue d ->
  Sem r ()
dequeueLoop errorDelay errorHandler queue = do
  QueueName name <- ask
  let
    spin =
      runStop (dequeue queue) >>= \case
        Right () -> spin
        Left err -> result =<< errorHandler err
    result = \case
      True ->
        disconnect *> Time.sleep errorDelay *> spin
      False -> do
        Log.warn [exon|Exiting dequeue loop for '##{name}' after error|]
        embed (atomically (closeTBMQueue queue))
    disconnect =
      unlisten *> resume_ Database.release
  spin

startDequeueLoop ::
  ∀ d t dt u r .
  Ord t =>
  TimeUnit u =>
  Members [RestartingMonitor, Reader QueueName] r =>
  Members [Store UUID (Queued t d) !! DbError, Databases, Time t dt, Log, Resource, Embed IO, Final IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  TBMQueue d ->
  Sem r ()
startDequeueLoop errorDelay errorHandler queue = do
  QueueName name <- ask
  withDatabaseUnique (Just (NamedTag [exon|dequeue-#{name}|])) do
    finally (restart (dequeueLoop errorDelay (insertAt @0 . errorHandler) queue)) unlisten

interpretInputQueue ::
  ∀ d r .
  Member (Embed IO) r =>
  TBMQueue d ->
  InterpreterFor (Input (Maybe d)) r
interpretInputQueue queue =
  interpret \case
    Input ->
      embed (atomically (readTBMQueue queue))

dequeueThread ::
  ∀ d t dt u r .
  Ord t =>
  TimeUnit u =>
  Members [RestartingMonitor, Reader QueueName, Resource] r =>
  Members [Store UUID (Queued t d) !! DbError, Databases, Time t dt, Log, Async, Embed IO, Final IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  Sem r (Concurrent.Async (Maybe ()), TBMQueue d)
dequeueThread errorDelay errorHandler = do
  queue <- embed (newTBMQueueIO 64)
  handle <- async (startDequeueLoop errorDelay errorHandler queue)
  pure (handle, queue)

interpretInputDbQueueListen ::
  ∀ (name :: Symbol) d t dt u r .
  Ord t =>
  TimeUnit u =>
  KnownSymbol name =>
  Members [RestartingMonitor, Final IO] r =>
  Members [Store UUID (Queued t d) !! DbError, Databases, Time t dt, Log, Resource, Async, Embed IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueListen errorDelay errorHandler sem =
  runReader (QueueName (symbolText @name)) $
  bracket acquire release \ (_, queue) -> do
    interpretInputQueue queue (raiseUnder sem)
  where
    acquire = dequeueThread errorDelay (raise . errorHandler)
    release (handle, _) = cancel handle

interpretInputQueueDb ::
  ∀ qname u t dt d diff r .
  TimeUnit u =>
  TimeUnit diff =>
  Torsor t diff =>
  Queue qname t =>
  Members [Store UUID (Queued t d) !! DbError, Databases] r =>
  Members [Time t dt, Log, Resource, Async, Race, Embed IO, Final IO] r =>
  u ->
  ClockSkewConfig ->
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputQueueDb errorDelay csConfig errorHandler =
  interpretAtomic Nothing .
  interpretMonitorRestart (monitorClockSkew csConfig) .
  raiseUnder .
  interpretInputDbQueueListen @qname errorDelay (insertAt @0 . errorHandler) .
  raiseUnder
