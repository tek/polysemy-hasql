module Polysemy.Hasql.Queue.Output where

import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Random (Random, random)
import Polysemy.Output (Output(Output))
import Polysemy.Tagged (Tagged, tag)
import qualified Polysemy.Time as Time
import Polysemy.Time (Seconds(Seconds), Time)
import Prelude hiding (group)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Hasql (HasqlConnection)
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.QueueOutputError as QueueOutputError
import Polysemy.Hasql.Data.QueueOutputError (QueueOutputError)
import qualified Polysemy.Hasql.Database as Database (retryingSql)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Queue.Data.Queue (OutputConn, Queue)
import Polysemy.Hasql.Queue.Data.Queued (QueueIdQuery(QueueIdQuery), Queued(Queued), QueuedRep)
import Polysemy.Hasql.Store (interpretStoreDbFullGenAs)

interpretOutputDbQueue ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) ! DbError, Database ! DbError, Time t dt, Random, Embed IO] r =>
  InterpreterFor (Output d ! QueueOutputError) r
interpretOutputDbQueue =
  interpretResumable \case
    Output d -> do
      id' <- random
      created <- Time.now
      resumeHoist QueueOutputError.Insert do
        Store.insert (Queued id' created d)
      resumeHoist QueueOutputError.Notify do
        Database.retryingSql (Seconds 3) (sql id')
        where
          sql id' =
            [qt|notify "#{symbolText @queue}", '#{id'}'|]

interpretOutputDbQueueFull ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Member (Tagged (OutputConn queue) HasqlConnection) r =>
  Members [Store UUID (Queued t d) ! DbError, Time t dt, Random, Embed IO] r =>
  InterpreterFor (Output d ! QueueOutputError) r
interpretOutputDbQueueFull =
  tag .
  interpretDatabase .
  interpretOutputDbQueue @queue .
  raiseUnder2

interpretOutputDbQueueFullGen ::
  ∀ (queue :: Symbol) d t dt r .
  Queue queue t d =>
  Members [Tagged (OutputConn queue) HasqlConnection, Database ! DbError, Time t dt, Random, Embed IO] r =>
  InterpreterFor (Output d ! QueueOutputError) r
interpretOutputDbQueueFullGen =
  interpretStoreDbFullGenAs @QueuedRep @(Queued t d) id id QueueIdQuery .
  raiseUnder2 .
  interpretOutputDbQueueFull @queue .
  raiseUnder
