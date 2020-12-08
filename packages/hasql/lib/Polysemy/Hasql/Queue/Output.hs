module Polysemy.Hasql.Queue.Output where

import Prelude hiding (group)
import qualified Database.PostgreSQL.LibPQ as LibPQ
import Hasql.Connection (withLibPQConnection)
import Polysemy.Db.Data.Column (Auto, Prim, UuidRep)
import qualified Polysemy.Db.Data.DbConnectionError as DbConnectionError
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.IdQuery (IdQuery)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UuidStore)
import Polysemy.Db.Data.Uid (Uid(Uid), Uuid)
import Polysemy.Output (Output(Output))
import Polysemy.Tagged (Tagged, tag)
import Polysemy.Time (Seconds(Seconds), Time)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Hasql (HasqlConnection)
import qualified Polysemy.Hasql.Data.Database as Database
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Database as Database (retryingSql)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Store (interpretStoreDbFullGenUid)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)

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
