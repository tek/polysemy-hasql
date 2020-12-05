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
import Polysemy.Resume (Stop, resumeHoist, stopEither, stopNote, stopToError)
import Polysemy.Tagged (Tagged, tag)

import Control.Concurrent (threadWaitRead)
import Control.Concurrent.STM.TBMQueue (TBMQueue, newTBMQueueIO, readTBMQueue, writeTBMQueue)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Hasql (HasqlConnection)
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.DbConnection as DbConnection
import qualified Polysemy.Hasql.Database as Database
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Resume (interpretResumable, restop, resume_, type (!))
import Polysemy.Time (GhcTime, Seconds(Seconds))

tryDequeue ::
  Show d =>
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
    Nothing -> do
      LibPQ.socket connection >>= \case
        Just fd -> do
          threadWaitRead fd
          Right Nothing <$ LibPQ.consumeInput connection
        Nothing ->
          pure (Left "couldn't connect with LibPQ.socket")

dequeueLoop ::
  Show d =>
  FromJSON d =>
  Members [Tagged conn HasqlConnection, Embed IO] r =>
  (Text -> Sem r ()) ->
  TBMQueue d ->
  Sem r ()
dequeueLoop errorHandler queue =
  forever do
    tag (resume_ (dequeueOne =<< DbConnection.connect))
  where
    dequeueOne connection = do
      join <$> tryAny (withLibPQConnection connection tryDequeue) >>= \case
        Right d ->
          traverse_ (embed . atomically . writeTBMQueue queue) d
        Left err ->
          raise (raise (errorHandler err))

interpretInputDbQueue ::
  ∀ d r .
  Show d =>
  FromJSON d =>
  Member (Embed IO) r =>
  TBMQueue d ->
  InterpreterFor (Input (Maybe d) ! DbError) r
interpretInputDbQueue queue =
  interpretResumable \case
    Input ->
      atomically (readTBMQueue queue)

dequeueThread ::
  Show d =>
  FromJSON d =>
  Members [Tagged conn HasqlConnection, Async, Embed IO] r =>
  (Text -> Sem r ()) ->
  Sem r (Concurrent.Async (Maybe ()), TBMQueue d)
dequeueThread errorHandler = do
  queue <- embed (newTBMQueueIO 64)
  handle <- async (dequeueLoop errorHandler queue)
  pure (handle, queue)

listen ::
  ∀ (queue :: Symbol) r .
  KnownSymbol queue =>
  Members [Database ! DbError, Stop DbError, Embed IO] r =>
  Sem r ()
listen =
  restop (Database.retryingSqlDef [qt|listen "#{symbolText @queue}"|])

releaseInputQueue ::
  ∀ (queue :: Symbol) d r .
  KnownSymbol queue =>
  Members [Database ! DbError, Async, Embed IO] r =>
  Concurrent.Async (Maybe ()) ->
  TBMQueue d ->
  Sem r ()
releaseInputQueue handle _ = do
  Async.cancel handle
  resume_ (Database.retryingSqlDef [qt|unlisten "#{symbolText @queue}"|])

interpretInputDbQueueListen ::
  ∀ (queue :: Symbol) (conn :: Symbol) d r .
  Show d =>
  FromJSON d =>
  KnownSymbol queue =>
  KnownSymbol conn =>
  Members [Database ! DbError, Tagged conn HasqlConnection, Resource, Async, Error DbError, Embed IO] r =>
  (Text -> Sem r ()) ->
  InterpreterFor (Input (Maybe d) ! DbError) r
interpretInputDbQueueListen errorHandler sem =
  bracket acquire (uncurry (releaseInputQueue @queue)) \ (_, queue) ->
    interpretInputDbQueue queue sem
  where
    acquire = do
      stopToError (listen @queue)
      dequeueThread errorHandler

interpretInputDbQueueFull ::
  ∀ (queue :: Symbol) (conn :: Symbol) d r .
  Show d =>
  FromJSON d =>
  KnownSymbol queue =>
  KnownSymbol conn =>
  Members [Tagged conn HasqlConnection, GhcTime, Resource, Async, Error DbError, Embed IO] r =>
  (Text -> Sem r ()) ->
  InterpreterFor (Input (Maybe d) ! DbError) r
interpretInputDbQueueFull errorHandler =
  tag @conn @HasqlConnection .
  interpretDatabase .
  interpretInputDbQueueListen @queue @conn (raise . raise . errorHandler) .
  raiseUnder2

escape ::
  Members [Database ! DbError, HasqlConnection, Stop DbError, Embed IO] r =>
  ByteString ->
  Sem r ByteString
escape payload = do
  connection <- resumeHoist (DbError.Connection) DbConnection.connect
  result <- tryAny (withLibPQConnection connection (flip LibPQ.escapeStringConn payload))
  stopNote invalid =<< stopEither (first connError result)
  where
    invalid =
      DbError.Query "invalid payload for notify"
    connError =
      DbError.Connection . DbConnectionError.Acquire

interpretOutputDbQueue ::
  ∀ (conn :: Symbol) d r .
  Show d =>
  ToJSON d =>
  KnownSymbol conn =>
  Members [Database ! DbError, HasqlConnection, GhcTime, Embed IO] r =>
  InterpreterFor (Output d ! DbError) r
interpretOutputDbQueue =
  interpretResumable \case
    Output d -> do
      payload <- escape (toStrict (Aeson.encode d))
      restop (Database.retryingSql (Seconds 3) [qt|notify "#{symbolText @conn}", '#{payload}'|])

interpretOutputDbQueueFull ::
  ∀ (conn :: Symbol) d r .
  Show d =>
  ToJSON d =>
  KnownSymbol conn =>
  Members [Tagged conn HasqlConnection, Error DbError, GhcTime, Embed IO] r =>
  InterpreterFor (Output d ! DbError) r
interpretOutputDbQueueFull =
  tag @conn @HasqlConnection .
  interpretDatabase .
  interpretOutputDbQueue @conn .
  raiseUnder2
