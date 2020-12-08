module Polysemy.Hasql.Queue where

import Prelude hiding (group)
import Control.Concurrent (threadWaitRead)
import qualified Control.Concurrent.Async as Concurrent
import Control.Concurrent.STM.TBMQueue (TBMQueue, closeTBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hasql.Connection (withLibPQConnection)
import qualified Polysemy.Async as Async
import Polysemy.Async (Async, async)
import Polysemy.Db.Data.Column (Auto, Prim, UuidRep)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.IdQuery (IdQuery)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UuidStore)
import Polysemy.Db.Data.Uid (Uid(Uid), Uuid)
import Polysemy.Input (Input(Input))
import Polysemy.Output (Output(Output))
import Polysemy.Resource (Resource, bracket)
import Polysemy.Resume (
  Stop,
  interpretResumable,
  restop,
  resume_,
  runStop,
  stopEither,
  stopEitherWith,
  stopNote,
  type (!),
  )
import Polysemy.Tagged (Tagged, tag)
import Polysemy.Time (Seconds(Seconds), Time)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Hasql (HasqlConnection)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb(InitDb))
import qualified Polysemy.Hasql.Database as Database (retryingSql, retryingSqlDef)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Store (interpretStoreDbFullGenUid)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)

tryDequeue ::
  LibPQ.Connection ->
  IO (Either Text (Maybe UUID))
tryDequeue connection =
  LibPQ.notifies connection >>= \case
    Just (LibPQ.Notify _ _ payload) ->
      case UUID.fromASCIIBytes payload of
        Just d -> do
          pure (Right (Just d))
        Nothing -> do
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

dequeue ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [UuidStore d ! DbError, Database ! DbError, Time t dt, Embed IO] r =>
  TBMQueue (Uuid d) ->
  Sem (Stop DbError : r) ()
dequeue queue = do
  restop @_ @Database $ Database.withInit (InitDb [qt|dequeue-#{symbolText @queue}|] (\ _ -> listen @queue)) do
    Database.connect \ connection -> do
      result <- join <$> tryAny (withLibPQConnection connection tryDequeue)
      void $ runMaybeT do
        id' <- MaybeT (stopEitherWith (DbError.Connection . DbConnectionError.Acquire) result)
        messages <- MaybeT (restop (Store.delete id'))
        lift (traverse_ (atomically . writeTBMQueue queue) messages)

dequeueLoop ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [UuidStore d ! DbError, Database ! DbError, Time t dt, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  TBMQueue (Uuid d) ->
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
  InterpreterFor (Input (Maybe d) ! DbError) r
interpretInputDbQueue queue =
  interpretResumable \case
    Input ->
      atomically (readTBMQueue queue)

dequeueThread ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [UuidStore d ! DbError, Database ! DbError, Time t dt, Async, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  Sem r (Concurrent.Async (Maybe ()), TBMQueue (Uuid d))
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

-- TODO acquire must be called with connectWithInit, otherwise it will not reconnect on connection loss
interpretInputDbQueueListen ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [UuidStore d ! DbError, Database ! DbError, Time t dt, Resource, Async, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe (Uuid d)) ! DbError) r
interpretInputDbQueueListen errorHandler sem =
  bracket acquire (uncurry (releaseInputQueue @queue)) \ (_, queue) ->
    interpretInputDbQueue queue sem
  where
    acquire =
      dequeueThread @queue errorHandler

interpretInputDbQueueFull ::
  ∀ (queue :: Symbol) (conn :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [Tagged conn HasqlConnection, UuidStore d ! DbError, Time t dt, Resource, Async, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe (Uuid d)) ! DbError) r
interpretInputDbQueueFull errorHandler =
  tag @conn @HasqlConnection .
  interpretDatabase .
  interpretInputDbQueueListen @queue (raise . raise . errorHandler) .
  raiseUnder2

interpretInputDbQueueFullGen ::
  ∀ (queue :: Symbol) (conn :: Symbol) d t dt r .
  KnownSymbol queue =>
  GenQueryTable (UuidRep Auto) (IdQuery UUID) (Uuid d) =>
  Members [Tagged conn HasqlConnection, Database ! DbError, Time t dt, Resource, Async, Embed IO] r =>
  (DbError -> Sem r Bool) ->
  InterpreterFor (Input (Maybe (Uuid d)) ! DbError) r
interpretInputDbQueueFullGen errorHandler =
  interpretStoreDbFullGenUid @Auto @(Prim Auto) .
  raiseUnder2 .
  interpretInputDbQueueFull @queue @conn (raise . errorHandler) .
  raiseUnder

escape ::
  Members [Database ! DbError, Stop DbError, Embed IO] r =>
  ByteString ->
  Sem r ByteString
escape payload = do
  restop $ Database.connect \ connection -> do
    result <- tryAny (withLibPQConnection connection (flip LibPQ.escapeStringConn payload))
    stopNote invalid =<< stopEither (first connError result)
  where
    invalid =
      DbError.Query "invalid payload for notify"
    connError =
      DbError.Connection . DbConnectionError.Acquire

interpretOutputDbQueue ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [UuidStore d ! DbError, Database ! DbError, Time t dt, Embed IO] r =>
  InterpreterFor (Output (Uuid d) ! DbError) r
interpretOutputDbQueue =
  interpretResumable \case
    Output d@(Uid id' _) -> do
      restop (Store.insert d)
      restop (Database.retryingSql (Seconds 3) [qt|notify "#{symbolText @queue}", '#{id'}'|])

interpretOutputDbQueueFull ::
  ∀ (conn :: Symbol) d t dt r .
  KnownSymbol conn =>
  Members [Tagged conn HasqlConnection, UuidStore d ! DbError, Time t dt, Embed IO] r =>
  InterpreterFor (Output (Uuid d) ! DbError) r
interpretOutputDbQueueFull =
  tag @conn @HasqlConnection .
  interpretDatabase .
  interpretOutputDbQueue @conn .
  raiseUnder2

interpretOutputDbQueueFullGen ::
  ∀ (conn :: Symbol) d t dt r .
  KnownSymbol conn =>
  GenQueryTable (UuidRep Auto) (IdQuery UUID) (Uuid d) =>
  Members [Tagged conn HasqlConnection, Database ! DbError, Time t dt, Embed IO] r =>
  InterpreterFor (Output (Uuid d) ! DbError) r
interpretOutputDbQueueFullGen =
  interpretStoreDbFullGenUid @Auto @(Prim Auto) .
  raiseUnder2 .
  interpretOutputDbQueueFull .
  raiseUnder
