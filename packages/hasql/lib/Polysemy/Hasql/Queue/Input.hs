module Polysemy.Hasql.Queue.Input where

import Control.Concurrent (threadWaitRead)
import qualified Control.Concurrent.Async as Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.UUID as UUID
import Data.UUID (UUID)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Exon (exon)
import Hasql.Connection (Connection, withLibPQConnection)
import qualified Polysemy.Async as Async
import qualified Polysemy.Conc as Monitor
import Polysemy.Conc (
  ClockSkewConfig,
  Monitor,
  Restart,
  RestartingMonitor,
  interpretAtomic,
  interpretMonitorRestart,
  monitorClockSkew,
  )
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uuid)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Final (withWeavingToFinal)
import Polysemy.Input (Input (Input))
import qualified Polysemy.Log as Log
import qualified Polysemy.Time as Time
import Prelude hiding (Queue, group, listen)
import Torsor (Torsor)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb (InitDb))
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode))
import qualified Polysemy.Hasql.Database as Database (retryingSqlDef)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Queue.Data.Queue (InputQueueConnection, Queue)
import Polysemy.Hasql.Queue.Data.Queued (Queued, QueuedRep)
import qualified Polysemy.Hasql.Queue.Data.Queued as Queued (Queued (..))
import Polysemy.Hasql.Store (interpretStoreDbFullNoUpdateGen)

tryDequeueSem ::
  Members [Monitor Restart, Embed IO] r =>
  LibPQ.Connection ->
  Sem r (Either Text (Maybe UUID))
tryDequeueSem connection =
  embed (LibPQ.notifies connection) >>= \case
    Just (LibPQ.Notify _ _ payload) ->
      case UUID.fromASCIIBytes payload of
        Just d ->
          pure (Right (Just d))
        Nothing ->
          pure (Left [exon|invalid UUID payload: #{decodeUtf8 payload}|])
    Nothing ->
      embed (LibPQ.socket connection) >>= \case
        Just fd -> do
          Monitor.monitor (embed (threadWaitRead fd))
          Right Nothing <$ embed (LibPQ.consumeInput connection)
        Nothing ->
          pure (Left "couldn't connect with LibPQ.socket")

listen ::
  ∀ (queue :: Symbol) r .
  KnownSymbol queue =>
  Members [Database, Log, Embed IO] r =>
  Sem r ()
listen = do
  Log.debug [exon|executing `listen` for queue #{symbolText @queue}|]
  Database.retryingSqlDef [exon|listen "#{SqlCode (symbolText @queue)}"|]

unlisten ::
  ∀ (queue :: Symbol) e r .
  KnownSymbol queue =>
  Members [Database !! e, Log] r =>
  Sem r ()
unlisten = do
  Log.debug [exon|executing `unlisten` for queue `#{symbolText @queue}`|]
  resume_ (Database.retryingSqlDef [exon|unlisten "#{SqlCode (symbolText @queue)}"|])

processMessages ::
  Ord t =>
  NonEmpty (Uuid (Queued t d)) ->
  NonEmpty d
processMessages =
  fmap (Queued.queue_payload . Uid._payload) . NonEmpty.sortWith (Queued.queue_created . Uid._payload)

initQueue ::
  ∀ (queue :: Symbol) e d t r .
  Ord t =>
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) !! e, Database, Log, Embed IO] r =>
  (d -> Sem r ()) ->
  Sem r ()
initQueue write = do
  waiting <- resumeAs Nothing Store.deleteAll
  traverse_ (traverse_ write . processMessages) waiting
  listen @queue

withPqConn ::
  Member (Final IO) r =>
  Connection ->
  (LibPQ.Connection -> Sem r a) ->
  Sem r (Either Text a)
withPqConn connection use =
  errorToIOFinal $ fromExceptionSemVia @SomeException show $ withWeavingToFinal \ s lower _ -> do
      withLibPQConnection connection \ c -> lower (raise (use c) <$ s)

dequeueAndProcess ::
  ∀ d t dt r .
  Ord t =>
  Members [Monitor Restart, Final IO] r =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Stop DbError, Log, Embed IO] r =>
  TBMQueue d ->
  Connection ->
  Sem r ()
dequeueAndProcess queue connection = do
  result <- join <$> withPqConn connection tryDequeueSem
  void $ runMaybeT do
    id' <- MaybeT (stopEitherWith (DbError.Connection . DbConnectionError.Acquire) result)
    messages <- MaybeT (restop (Store.delete id'))
    liftIO (traverse_ (atomically . writeTBMQueue queue) (processMessages messages))

dequeue ::
  ∀ (queue :: Symbol) d t dt r .
  Ord t =>
  KnownSymbol queue =>
  Members [Monitor Restart, Final IO] r =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Stop DbError, Time t dt, Log, Embed IO] r =>
  TBMQueue d ->
  Sem r ()
dequeue queue =
  restop @_ @Database (Database.withInit initDb (Database.connect (dequeueAndProcess queue)))
  where
    initDb =
      InitDb [exon|dequeue-#{name}|] \ _ ->
        initQueue @queue (embed . atomically . writeTBMQueue queue)
    name =
      symbolText @queue

dequeueLoop ::
  ∀ (queue :: Symbol) d t dt u resource r .
  Ord t =>
  TimeUnit u =>
  KnownSymbol queue =>
  Member (RestartingMonitor resource) r =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Embed IO, Final IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  TBMQueue d ->
  Sem r ()
dequeueLoop errorDelay errorHandler queue =
  Monitor.restart spin
  where
    spin =
      either (result <=< raise . errorHandler) (const spin) =<< runStop (dequeue @queue queue)
    result = \case
      True -> disconnect *> Time.sleep errorDelay *> spin
      False -> embed (atomically (closeTBMQueue queue))
    disconnect =
      unlisten @queue *> resume_ Database.disconnect

interpretInputDbQueue ::
  ∀ d r .
  Member (Embed IO) r =>
  TBMQueue d ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueue queue =
  interpret \case
    Input ->
      embed (atomically (readTBMQueue queue))

dequeueThread ::
  ∀ (queue :: Symbol) d t dt u resource r .
  Ord t =>
  TimeUnit u =>
  KnownSymbol queue =>
  Member (RestartingMonitor resource) r =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Async, Embed IO, Final IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  Sem r (Concurrent.Async (Maybe ()), TBMQueue d)
dequeueThread errorDelay errorHandler = do
  queue <- embed (newTBMQueueIO 64)
  handle <- async (dequeueLoop @queue errorDelay errorHandler queue)
  pure (handle, queue)

releaseInputQueue ::
  ∀ (queue :: Symbol) d e r .
  KnownSymbol queue =>
  Members [Database !! e, Log, Async, Embed IO] r =>
  Concurrent.Async (Maybe ()) ->
  TBMQueue d ->
  Sem r ()
releaseInputQueue handle _ = do
  Log.debug [exon|executing `unlisten` for queue `#{symbolText @queue}`|]
  Async.cancel handle
  resume_ (Database.retryingSqlDef [exon|unlisten "#{SqlCode (symbolText @queue)}"|])

interpretInputDbQueueListen ::
  ∀ (queue :: Symbol) d t dt u resource r .
  Ord t =>
  TimeUnit u =>
  KnownSymbol queue =>
  Members [RestartingMonitor resource, Final IO] r =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Resource, Async, Embed IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueListen errorDelay errorHandler sem =
  bracket acquire (uncurry (releaseInputQueue @queue)) \ (_, queue) ->
    interpretInputDbQueue queue sem
  where
    acquire =
      dequeueThread @queue errorDelay errorHandler

interpretInputDbQueueFull ::
  ∀ (queue :: Symbol) d t diff dt u r .
  Ord t =>
  TimeUnit u =>
  TimeUnit diff =>
  Torsor t diff =>
  KnownSymbol queue =>
  Members [InputQueueConnection queue, Store UUID (Queued t d) !! DbError, Time t dt, Log, Race, Resource, Async] r =>
  Members [Embed IO, Final IO] r =>
  u ->
  ClockSkewConfig ->
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueFull errorDelay csConfig errorHandler =
  tag .
  interpretDatabase .
  interpretAtomic Nothing .
  interpretMonitorRestart (monitorClockSkew csConfig) .
  raiseUnder .
  interpretInputDbQueueListen @queue errorDelay (raise . raise . raise . errorHandler) .
  raiseUnder3

interpretInputDbQueueFullGen ::
  ∀ (queue :: Symbol) d t diff dt u r .
  TimeUnit u =>
  TimeUnit diff =>
  Torsor t diff =>
  Queue queue t d =>
  Members [InputQueueConnection queue, Database !! DbError, Time t dt, Log, Resource, Async, Embed IO, Final IO] r =>
  Member Race r =>
  u ->
  ClockSkewConfig ->
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueFullGen errorDelay csConfig errorHandler =
  interpretStoreDbFullNoUpdateGen @QueuedRep .
  raiseUnder2 .
  interpretInputDbQueueFull @queue errorDelay csConfig (raise . errorHandler) .
  raiseUnder
