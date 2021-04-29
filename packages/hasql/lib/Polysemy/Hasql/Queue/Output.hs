module Polysemy.Hasql.Queue.Output where

import Polysemy.Db.Data.Column (PrimQuery)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Random (Random, random)
import Polysemy.Db.SOP.Constraint (symbolText)
import qualified Polysemy.Log as Log
import Polysemy.Log (Log)
import Polysemy.Output (Output(Output))
import Polysemy.Tagged (tag)
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds(Seconds), Time)
import Prelude hiding (group)

import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.QueueOutputError as QueueOutputError
import Polysemy.Hasql.Data.QueueOutputError (QueueOutputError)
import qualified Polysemy.Hasql.Database as Database (retryingSql)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Queue.Data.Queue (OutputQueueConnection, Queue)
import Polysemy.Hasql.Queue.Data.Queued (Queued(Queued), QueuedRep)
import Polysemy.Hasql.Store (interpretStoreDbFullGen)

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
        Store.insert (Queued id' created d)
      Log.debug [qt|executing `notify` for queue #{symbolText @queue}|]
      resumeHoist QueueOutputError.Notify do
        Database.retryingSql (Seconds 3) (sql id')
        where
          sql id' =
            [qt|notify "#{symbolText @queue}", '#{id'}'|]

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
  interpretStoreDbFullGen @(PrimQuery "queue_id") @QueuedRep .
  raiseUnder2 .
  interpretOutputDbQueueFull @queue .
  raiseUnder
