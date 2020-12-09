module Polysemy.Hasql.Queue.Input where

import Control.Concurrent (threadWaitRead)
import qualified Control.Concurrent.Async as Concurrent
import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.LibPQ as LibPQ
import GHC.TypeLits (AppendSymbol)
import Hasql.Connection (withLibPQConnection)
import qualified Polysemy.Async as Async
import Polysemy.Async (Async, async)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Input (Input(Input))
import Polysemy.Resource (Resource, bracket)
import Polysemy.Tagged (Tagged, tag)
import Polysemy.Time (Time)
import Prelude hiding (group)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Hasql (HasqlConnection)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb(InitDb))
import qualified Polysemy.Hasql.Database as Database (retryingSqlDef)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Queue.Data.Queued (QueueIdQuery(QueueIdQuery), Queued, QueuedRep)
import qualified Polysemy.Hasql.Queue.Data.Queued as Queued (Queued(..))
import Polysemy.Hasql.Store (interpretStoreDbFullGenAs)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)

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
          pure (Left [qt|invalid UUID payload: #{payload}|])
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
  Members [Database ! e, Stop e, Embed IO] r =>
  Sem r ()
listen =
  restop (Database.retryingSqlDef [qt|listen "#{symbolText @queue}"|])

processMessages ::
  Ord t =>
  NonEmpty (Queued t d) ->
  NonEmpty d
processMessages =
  fmap Queued.queue_payload . NonEmpty.sortWith Queued.queue_created

initQueue ::
  ∀ (queue :: Symbol) e d t r .
  Ord t =>
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) ! e, Database ! e, Stop e, Embed IO] r =>
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
  Members [Store UUID (Queued t d) ! DbError, Database ! DbError, Time t dt, Embed IO] r =>
  TBMQueue d ->
  Sem (Stop DbError : r) ()
dequeue queue =
  restop @_ @Database $ Database.withInit (InitDb [qt|dequeue-#{symbolText @queue}|] (\ _ -> initQueue @queue write)) do
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
  ∀ (queue :: Symbol) d t dt r .
  Ord t =>
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) ! DbError, Database ! DbError, Time t dt, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  TBMQueue d ->
  Sem r ()
dequeueLoop errorHandler queue =
  spin
  where
    spin =
      result =<< bitraverse errorHandler pure =<< runStop (dequeue @queue queue)
    result = \case
      Right () -> spin
      Left True -> spin
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
  ∀ (queue :: Symbol) d t dt r .
  Ord t =>
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) ! DbError, Database ! DbError, Time t dt, Async, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  Sem r (Concurrent.Async (Maybe ()), TBMQueue d)
dequeueThread errorHandler = do
  queue <- embed (newTBMQueueIO 64)
  handle <- async (dequeueLoop @queue errorHandler queue)
  pure (handle, queue)

releaseInputQueue ::
  ∀ (queue :: Symbol) d e r .
  KnownSymbol queue =>
  Members [Database ! e, Async, Embed IO] r =>
  Concurrent.Async (Maybe ()) ->
  TBMQueue d ->
  Sem r ()
releaseInputQueue handle _ = do
  Async.cancel handle
  resume_ (Database.retryingSqlDef [qt|unlisten "#{symbolText @queue}"|])

interpretInputDbQueueListen ::
  ∀ (queue :: Symbol) d t dt r .
  Ord t =>
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) ! DbError, Database ! DbError, Time t dt, Resource, Async, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueListen errorHandler sem =
  bracket acquire (uncurry (releaseInputQueue @queue)) \ (_, queue) ->
    interpretInputDbQueue queue sem
  where
    acquire =
      dequeueThread @queue errorHandler

type Conn queue =
  AppendSymbol queue "-input"

interpretInputDbQueueFull ::
  ∀ (queue :: Symbol) d t dt r .
  Ord t =>
  KnownSymbol queue =>
  Members [Tagged (Conn queue) HasqlConnection, Store UUID (Queued t d) ! DbError, Time t dt, Resource, Async] r =>
  Member (Embed IO) r =>
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueFull errorHandler =
  tag @(Conn queue) @HasqlConnection .
  interpretDatabase .
  interpretInputDbQueueListen @queue (raise . raise . errorHandler) .
  raiseUnder2

interpretInputDbQueueFullGen ::
  ∀ (queue :: Symbol) rep d t dt r .
  Ord t =>
  KnownSymbol queue =>
  GenQueryTable (QueuedRep rep) QueueIdQuery (Queued t d) =>
  Members [Tagged (Conn queue) HasqlConnection, Database ! DbError, Time t dt, Resource, Async, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe d)) r
interpretInputDbQueueFullGen errorHandler =
  interpretStoreDbFullGenAs @(QueuedRep rep) @(Queued t d) id id QueueIdQuery .
  raiseUnder2 .
  interpretInputDbQueueFull @queue (raise . errorHandler) .
  raiseUnder
