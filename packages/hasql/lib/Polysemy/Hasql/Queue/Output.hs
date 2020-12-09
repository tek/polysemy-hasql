module Polysemy.Hasql.Queue.Output where

import Prelude hiding (group)
import Polysemy.Db.Data.Column (Auto, Prim, UuidRep)
import Polysemy.Db.Data.IdQuery (IdQuery)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UuidStore)
import Polysemy.Db.Data.Uid (Uid(Uid), Uuid)
import Polysemy.Output (Output(Output))
import Polysemy.Tagged (Tagged, tag)
import Polysemy.Time (Seconds(Seconds), Time)
import GHC.TypeLits (AppendSymbol)

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Hasql (HasqlConnection)
import Polysemy.Hasql.Data.Database (Database)
import qualified Polysemy.Hasql.Data.QueueOutputError as QueueOutputError
import Polysemy.Hasql.Data.QueueOutputError (QueueOutputError)
import qualified Polysemy.Hasql.Database as Database (retryingSql)
import Polysemy.Hasql.Database (interpretDatabase)
import Polysemy.Hasql.Store (interpretStoreDbFullGenUid)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)

interpretOutputDbQueue ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [UuidStore d ! DbError, Database ! DbError, Time t dt, Embed IO] r =>
  InterpreterFor (Output (Uuid d) ! QueueOutputError) r
interpretOutputDbQueue =
  interpretResumable \case
    Output d@(Uid id' _) -> do
      resumeHoist QueueOutputError.Insert do
        Store.insert d
      resumeHoist QueueOutputError.Notify do
        Database.retryingSql (Seconds 3) sql
        where
          sql =
            [qt|notify "#{symbolText @queue}", '#{id'}'|]

interpretOutputDbQueueFull ::
  ∀ (queue :: Symbol) (conn :: Symbol) d t dt r .
  KnownSymbol queue =>
  Members [Tagged conn HasqlConnection, UuidStore d ! DbError, Time t dt, Embed IO] r =>
  InterpreterFor (Output (Uuid d) ! QueueOutputError) r
interpretOutputDbQueueFull =
  tag @conn @HasqlConnection .
  interpretDatabase .
  interpretOutputDbQueue @queue .
  raiseUnder2

type Conn queue =
  AppendSymbol queue "-output"

interpretOutputDbQueueFullGen ::
  ∀ (queue :: Symbol) d t dt r .
  KnownSymbol queue =>
  GenQueryTable (UuidRep Auto) (IdQuery UUID) (Uuid d) =>
  Members [Tagged (Conn queue) HasqlConnection, Database ! DbError, Time t dt, Embed IO] r =>
  InterpreterFor (Output (Uuid d) ! QueueOutputError) r
interpretOutputDbQueueFullGen =
  interpretStoreDbFullGenUid @Auto @(Prim Auto) .
  raiseUnder2 .
  interpretOutputDbQueueFull @queue @(Conn queue) .
  raiseUnder
