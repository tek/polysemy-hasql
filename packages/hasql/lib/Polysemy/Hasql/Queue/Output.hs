module Polysemy.Hasql.Queue.Output where

import GHC.TypeLits (AppendSymbol)
import Polysemy.Db.Data.IdQuery (IdQuery(IdQuery))
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
import Polysemy.Hasql.Queue.Data.Queued (Queued(Queued), QueuedRep)
import Polysemy.Hasql.Store (interpretStoreDbFullGenAs)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)

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
  ∀ (queue :: Symbol) (conn :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [Tagged conn HasqlConnection, Store UUID (Queued t d) ! DbError, Time t dt, Random, Embed IO] r =>
  InterpreterFor (Output d ! QueueOutputError) r
interpretOutputDbQueueFull =
  tag @conn @HasqlConnection .
  interpretDatabase .
  interpretOutputDbQueue @queue .
  raiseUnder2

type Conn queue =
  AppendSymbol queue "-output"

interpretOutputDbQueueFullGen ::
  ∀ (queue :: Symbol) rep d t dt r .
  KnownSymbol queue =>
  GenQueryTable (QueuedRep rep) (IdQuery UUID) (Queued t d) =>
  Members [Tagged (Conn queue) HasqlConnection, Database ! DbError, Time t dt, Random, Embed IO] r =>
  InterpreterFor (Output d ! QueueOutputError) r
interpretOutputDbQueueFullGen =
  interpretStoreDbFullGenAs @(QueuedRep rep) @(Queued t d) id id IdQuery .
  raiseUnder2 .
  interpretOutputDbQueueFull @queue @(Conn queue) .
  raiseUnder
