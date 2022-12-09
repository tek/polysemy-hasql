module Polysemy.Hasql.Queue.Output where

import Data.UUID (UUID)
import Exon (exon)
import Generics.SOP (NP (Nil, (:*)))
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Db.Random (Random, random)
import qualified Polysemy.Log as Log
import Polysemy.Output (Output (Output))
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds (Seconds))
import Prelude hiding (Queue, group)
import Sqel.Codec (PrimColumn)
import Sqel.Data.Dd (DbTypeName)
import Sqel.Data.Sql (Sql (Sql))
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (tableSchema)
import qualified Sqel.Prim as Sqel
import Sqel.Prim (prim, primAs)
import Sqel.Product (uid)
import Sqel.Query (checkQuery)
import Sqel.SOP.Constraint (symbolText)

import qualified Polysemy.Hasql.Data.QueueOutputError as QueueOutputError
import Polysemy.Hasql.Data.QueueOutputError (QueueOutputError)
import qualified Polysemy.Hasql.Database as Database (retryingSql)
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.Store (interpretDbTable, interpretStoreDb)
import Polysemy.Hasql.Queue.Data.Queue (Queue)
import Polysemy.Hasql.Queue.Data.Queued (Queued (Queued))

-- TODO I think notify doesn't even need a unique connection, only listen does
interpretOutputDbQueue ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Random, Embed IO] r =>
  InterpreterFor (Output d !! QueueOutputError) r
interpretOutputDbQueue =
  interpretResumable \case
    Output d -> do
      id' <- random
      created <- Time.now
      resumeHoist QueueOutputError.Insert do
        Store.insert (Uid id' (Queued created d))
      Log.debug [exon|executing `notify` for queue '#{symbolText @queue}'|]
      resumeHoist QueueOutputError.Notify do
        Database.retryingSql (Seconds 3) (sql id')
      where
        sql id' =
          [exon|notify "#{Sql (symbolText @queue)}", '#{show id'}'|]

interpretOutputDbQueueFull ::
  ∀ (queue :: Symbol) d t dt r name .
  ToJSON d =>
  FromJSON d =>
  PrimColumn t =>
  Queue queue t =>
  DbTypeName d name =>
  KnownSymbol (AppendSymbol "Queued" name) =>
  Members [Database !! DbError, Time t dt, Log, Random, Resource, Embed IO] r =>
  InterpreterFor (Output d !! QueueOutputError) r
interpretOutputDbQueueFull =
  interpretDbTable ts .
  interpretStoreDb ts (checkQuery query table) .
  raiseUnder .
  interpretOutputDbQueue @queue .
  raiseUnder
  where
    ts = tableSchema table
    query = primAs @"id"
    table = uid prim (prim :* Sqel.json :* Nil)
