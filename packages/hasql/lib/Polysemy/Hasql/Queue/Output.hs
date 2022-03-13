module Polysemy.Hasql.Queue.Output where

import Data.UUID (UUID)
import Exon (exon)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.Uid (Uid (Uid))
import Polysemy.Db.Random (Random, random)
import Polysemy.Db.SOP.Constraint (symbolText)
import qualified Polysemy.Log as Log
import Polysemy.Output (Output (Output))
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds (Seconds))
import Prelude hiding (Queue, group)

import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.QueueOutputError as QueueOutputError
import Polysemy.Hasql.Data.QueueOutputError (QueueOutputError)
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode))
import qualified Polysemy.Hasql.Database as Database (retryingSql)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Queue.Data.Queue (OutputQueueConnection, Queue)
import Polysemy.Hasql.Queue.Data.Queued (Queued (Queued), QueuedRep)
import Polysemy.Hasql.Store (interpretStoreDbFullNoUpdateGen)

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
      Log.debug [exon|executing `notify` for queue #{symbolText @queue}|]
      resumeHoist QueueOutputError.Notify do
        Database.retryingSql (Seconds 3) (sql id')
        where
          sql id' =
            [exon|notify "#{SqlCode (symbolText @queue)}", '#{show id'}'|]

interpretOutputDbQueueFull ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Member (OutputQueueConnection queue) r =>
  Members [Store UUID (Queued t d) !! DbError, Time t dt, Log, Random, Embed IO] r =>
  InterpreterFor (Output d !! QueueOutputError) r
interpretOutputDbQueueFull =
  tag .
  interpretDatabase .
  interpretOutputDbQueue @queue .
  raiseUnder2

interpretOutputDbQueueFullGen ::
  ∀ (queue :: Symbol) d t dt r .
  Queue queue t d =>
  Members [OutputQueueConnection queue, Database !! DbError, Time t dt, Log, Random, Embed IO] r =>
  InterpreterFor (Output d !! QueueOutputError) r
interpretOutputDbQueueFullGen =
  interpretStoreDbFullNoUpdateGen @QueuedRep .
  raiseUnder2 .
  interpretOutputDbQueueFull @queue .
  raiseUnder
