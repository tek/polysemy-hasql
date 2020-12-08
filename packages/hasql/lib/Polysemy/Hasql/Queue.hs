module Polysemy.Hasql.Queue where

import Prelude hiding (group)
import qualified Control.Concurrent.Async as Concurrent
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hasql.Connection (withLibPQConnection)
import qualified Polysemy.Async as Async
import Polysemy.Async (Async, async)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Input (Input(Input))
import Polysemy.Output (Output(Output))
import Polysemy.Resource (Resource, bracket)
import Polysemy.Resume (Stop, runStop, stopEither, stopEitherWith, stopNote)
import Polysemy.Tagged (Tagged, tag)

import Control.Concurrent (threadWaitRead)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Hasql (HasqlConnection)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database, InitDb(InitDb))
import qualified Polysemy.Hasql.Database as Database (retryingSql, retryingSqlDef)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Resume (interpretResumable, restop, resume_, type (!))
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds(Seconds), Time)

tryDequeue ::
  FromJSON d =>
  LibPQ.Connection ->
  IO (Either Text (Maybe d))
tryDequeue connection =
  LibPQ.notifies connection >>= \case
    Just (LibPQ.Notify _ _ payload) ->
      case Aeson.eitherDecodeStrict' payload of
        Right d -> do
          pure (Right (Just d))
        Left err -> do
          pure (Left (toText err))
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
  FromJSON d =>
  KnownSymbol queue =>
  Members [Database ! DbError, Time t dt, Embed IO] r =>
  TBMQueue d ->
  Sem (Stop DbError : r) ()
dequeue queue = do
  restop $ Database.withInit (InitDb [qt|dequeue-#{symbolText @queue}|] (\ _ -> listen @queue)) do
    Database.connect \ connection -> do
      Time.sleep (Seconds 1)
      result <- join <$> tryAny (withLibPQConnection connection tryDequeue)
      d <- stopEitherWith (DbError.Connection . DbConnectionError.Acquire) result
      traverse_ (embed . atomically . writeTBMQueue queue) d

dequeueLoop ::
  ∀ (queue :: Symbol) d t dt r .
  FromJSON d =>
  KnownSymbol queue =>
  Members [Database ! DbError, Time t dt, Embed IO] r =>
  (DbError -> Sem r ()) ->
  TBMQueue d ->
  Sem r ()
dequeueLoop errorHandler queue =
  forever do
    traverseLeft errorHandler =<< runStop (dequeue @queue queue)

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
  FromJSON d =>
  KnownSymbol queue =>
  Members [Database ! DbError, Time t dt, Async, Embed IO] r =>
  (DbError -> Sem r ()) ->
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

-- TODO acquire must be called with connectWithInit, otherwise it will not reconnect on connection loss
interpretInputDbQueueListen ::
  ∀ (queue :: Symbol) d t dt r .
  FromJSON d =>
  KnownSymbol queue =>
  Members [Database ! DbError, Time t dt, Resource, Async, Embed IO] r =>
  (DbError -> Sem r ()) ->
  InterpreterFor (Input (Maybe d) ! DbError) r
interpretInputDbQueueListen errorHandler sem =
  bracket acquire (uncurry (releaseInputQueue @queue)) \ (_, queue) ->
    interpretInputDbQueue queue sem
  where
    acquire =
      dequeueThread @queue errorHandler

interpretInputDbQueueFull ::
  ∀ (queue :: Symbol) (conn :: Symbol) d t dt r .
  FromJSON d =>
  KnownSymbol queue =>
  Members [Tagged conn HasqlConnection, Time t dt, Resource, Async, Embed IO] r =>
  (DbError -> Sem r ()) ->
  InterpreterFor (Input (Maybe d) ! DbError) r
interpretInputDbQueueFull errorHandler =
  tag @conn @HasqlConnection .
  interpretDatabase .
  interpretInputDbQueueListen @queue (raise . raise . errorHandler) .
  raiseUnder2

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
  ToJSON d =>
  KnownSymbol queue =>
  Members [Database ! DbError, Time t dt, Embed IO] r =>
  InterpreterFor (Output d ! DbError) r
interpretOutputDbQueue =
  interpretResumable \case
    Output d -> do
      payload <- escape (toStrict (Aeson.encode d))
      restop (Database.retryingSql (Seconds 3) [qt|notify "#{symbolText @queue}", '#{payload}'|])

interpretOutputDbQueueFull ::
  ∀ (conn :: Symbol) d t dt r .
  ToJSON d =>
  KnownSymbol conn =>
  Members [Tagged conn HasqlConnection, Time t dt, Embed IO] r =>
  InterpreterFor (Output d ! DbError) r
interpretOutputDbQueueFull =
  tag @conn @HasqlConnection .
  interpretDatabase .
  interpretOutputDbQueue @conn .
  raiseUnder2
