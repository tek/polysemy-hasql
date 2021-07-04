module Polysemy.Hasql.Queue.Input where

import Control.Concurrent (threadWaitRead)
import qualified Control.Concurrent.Async as Concurrent
import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hasql.Connection (withLibPQConnection)
import qualified Polysemy.Async as Async
import Polysemy.Async (Async, async)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uuid)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Input (Input (Input))
import qualified Polysemy.Log as Log
import Polysemy.Log (Log)
import Polysemy.Resource (Resource, bracket)
import Polysemy.Tagged (tag)
import qualified Polysemy.Time as Time
import Polysemy.Time (Time, TimeUnit)
import Prelude hiding (group)

import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb (InitDb))
import qualified Polysemy.Hasql.Database as Database (retryingSqlDef)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Queue.Data.Queue (InputQueueConnection, Queue)
import Polysemy.Hasql.Queue.Data.Queued (Queued, QueuedRep)
import qualified Polysemy.Hasql.Queue.Data.Queued as Queued (Queued (..))
import Polysemy.Hasql.Store (interpretStoreDbFullNoUpdateGen)

tryDequeue ::
  LibPQ.Connection ->
  IO (Either Text (Maybe UUID))
tryDequeue connection =
  LibPQ.notifies connection >>= \case
    Just (LibPQ.Notify _ _ payload) ->
      case UUID.fromASCIIBytes payload of
        Just d ->
          pure (Right (Just d))
        Nothing ->
          pure (Left [text|invalid UUID payload: #{payload}|])
    Nothing ->
      LibPQ.socket connection >>= \case
        Just fd -> do
          threadWaitRead fd
          Right Nothing <$ LibPQ.consumeInput connection
        Nothing ->
          pure (Left "couldn't connect with LibPQ.socket")

listen ::
  ∀ (queue :: Symbol) e r .
  KnownSymbol queue =>
  Members [Database !! e, Stop e, Log, Embed IO] r =>
  Sem r ()
listen = do
  Log.debug [text|executing `listen` for queue #{symbolText @queue}|]
  restop (Database.retryingSqlDef [text|listen "#{symbolText @queue}"|])

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
  Members [Store UUID (Queued t d) !! e, Database !! e, Log, Stop e, Embed IO] r =>
  (d -> Sem r ()) ->
  Sem r ()
initQueue write = do
  waiting <- resumeAs Nothing Store.deleteAll
  traverse_ (traverse_ write . processMessages) waiting
  listen @queue

dequeue ::
  ∀ (queue :: Symbol) d t dt r .
  Ord t =>
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Embed IO] r =>
  TBMQueue d ->
  Sem (Stop DbError : r) ()
dequeue queue =
  restop @_ @Database $ Database.withInit (InitDb [text|dequeue-#{symbolText @queue}|] (\ _ -> initQueue @queue write)) do
    Database.connect \ connection -> do
      result <- join <$> tryAny (withLibPQConnection connection tryDequeue)
      void $ runMaybeT do
        id' <- MaybeT (stopEitherWith (DbError.Connection . DbConnectionError.Acquire) result)
        messages <- MaybeT (restop (Store.delete id'))
        lift (traverse_ write (processMessages messages))
  where
    write =
      atomically . writeTBMQueue queue

dequeueLoop ::
  ∀ (queue :: Symbol) d t dt u r .
  Ord t =>
  TimeUnit u =>
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Embed IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  TBMQueue d ->
  Sem r ()
dequeueLoop errorDelay errorHandler queue =
  spin
  where
    spin =
      result =<< bitraverse errorHandler pure =<< runStop (dequeue @queue queue)
    result = \case
      Right () -> spin
      Left True -> Time.sleep errorDelay >> spin
      Left False -> atomically (closeTBMQueue queue)

interpretInputDbQueue ::
  ∀ d r .
  Member (Embed IO) r =>
  TBMQueue d ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueue queue =
  interpret \case
    Input ->
      atomically (readTBMQueue queue)

dequeueThread ::
  ∀ (queue :: Symbol) d t dt u r .
  Ord t =>
  TimeUnit u =>
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Async, Embed IO] r =>
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
  Log.debug [text|executing `unlisten` for queue `#{symbolText @queue}`|]
  Async.cancel handle
  resume_ (Database.retryingSqlDef [text|unlisten "#{symbolText @queue}"|])

interpretInputDbQueueListen ::
  ∀ (queue :: Symbol) d t dt u r .
  Ord t =>
  TimeUnit u =>
  KnownSymbol queue =>
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
  ∀ (queue :: Symbol) d t dt u r .
  Ord t =>
  TimeUnit u =>
  KnownSymbol queue =>
  Members [InputQueueConnection queue, Store UUID (Queued t d) !! DbError, Time t dt, Log, Resource, Async] r =>
  Member (Embed IO) r =>
  u ->
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueFull errorDelay errorHandler =
  tag .
  interpretDatabase .
  interpretInputDbQueueListen @queue errorDelay (raise . raise . errorHandler) .
  raiseUnder2

interpretInputDbQueueFullGen ::
  ∀ (queue :: Symbol) d tree t dt u r .
  TimeUnit u =>
  Queue queue t d tree =>
  Members [InputQueueConnection queue, Database !! DbError, Time t dt, Log, Resource, Async, Embed IO] r =>
  u ->
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueFullGen errorDelay errorHandler =
  interpretStoreDbFullNoUpdateGen @QueuedRep .
  raiseUnder2 .
  interpretInputDbQueueFull @queue errorDelay (raise . errorHandler) .
  raiseUnder
