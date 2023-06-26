module Polysemy.Hasql.Queue.Output where

import Data.UUID (UUID)
import Exon (exon)
import qualified Log
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Effect.Random (Random, random)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Db.Symbol (symbolText)
import Polysemy.Output (Output (Output))
import Prelude hiding (Queue, group)
import Sqel (Uid (Uid))
import Sqel.Exts (Sql (Sql))
import qualified Time as Time
import Time (Seconds (Seconds))

import qualified Polysemy.Hasql.Data.QueueOutputError as QueueOutputError
import Polysemy.Hasql.Data.QueueOutputError (QueueOutputError)
import qualified Polysemy.Hasql.Database as Database (retryingSql)
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Queue.Data.Queued (Queued (Queued))

-- TODO I think notify doesn't even need a unique connection, only listen does
interpretOutputQueueDb ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [Store UUID (Queued t d) !! DbError, Database !! DbError, Time t dt, Log, Random UUID, Embed IO] r =>
  InterpreterFor (Output d !! QueueOutputError) r
interpretOutputQueueDb =
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
