module Polysemy.Hasql.Queue.Data.Queue where

import GHC.TypeLits (AppendSymbol)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Tagged (Tagged)

import Polysemy.Hasql (HasqlConnection)
import Polysemy.Hasql.Queue.Data.Queued (Queued, QueuedRep)
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (UuidSchema)

type family InputConn (queue :: Symbol) :: Symbol where
  InputConn queue =
    AppendSymbol queue "-input"

type family OutputConn (queue :: Symbol) :: Symbol where
  OutputConn queue =
    AppendSymbol queue "-output"

type family Queue (queue :: Symbol) t d (tree :: Kind.Tree) :: Constraint where
  Queue queue t d tree =
    (
      Ord t,
      KnownSymbol queue,
      KnownSymbol (InputConn queue),
      KnownSymbol (OutputConn queue),
      UuidSchema QueuedRep (Queued t d),
      BuildPartialSql (Queued t d) tree
    )

type family QueueInput (queue :: Symbol) t d :: Constraint where
  QueueInput queue t d =
    (Ord t, KnownSymbol (InputConn queue), UuidSchema QueuedRep (Queued t d))

type family QueueOutput (queue :: Symbol) t d :: Constraint where
  QueueOutput queue t d =
    (Ord t, KnownSymbol (OutputConn queue), UuidSchema QueuedRep (Queued t d))

type family InputQueueConnection (queue :: symbol) :: Effect where
  InputQueueConnection queue =
    Tagged (InputConn queue) HasqlConnection

type family OutputQueueConnection (queue :: symbol) :: Effect where
  OutputQueueConnection queue =
    Tagged (OutputConn queue) HasqlConnection

inputQueueName ::
  ∀ (queue :: Symbol) .
  KnownSymbol (InputConn queue) =>
  Text
inputQueueName =
  symbolText @(InputConn queue)

outputQueueName ::
  ∀ (queue :: Symbol) .
  KnownSymbol (OutputConn queue) =>
  Text
outputQueueName =
  symbolText @(OutputConn queue)
