module Polysemy.Hasql.Queue.Data.Queue where

import GHC.TypeLits (AppendSymbol)
import Polysemy.Db.Data.Column (Auto, PrimQuery)
import Polysemy.Db.SOP.Constraint (symbolText)
import Polysemy.Hasql.Table.Schema (Schema)
import Polysemy.Tagged (Tagged)

import Polysemy.Hasql (HasqlConnection)
import Polysemy.Hasql.Queue.Data.Queued (QueueIdQuery, Queued, QueuedRep)

type family InputConn (queue :: Symbol) :: Symbol where
  InputConn queue =
    AppendSymbol queue "-input"

type family OutputConn (queue :: Symbol) :: Symbol where
  OutputConn queue =
    AppendSymbol queue "-output"

type family Queue (queue :: Symbol) t d :: Constraint where
  Queue queue t d =
    (
      Ord t,
      KnownSymbol queue,
      KnownSymbol (InputConn queue),
      KnownSymbol (OutputConn queue),
      Schema (PrimQuery "queue_id") QueuedRep UUID (Queued t d)
    )

type family QueueInput (queue :: Symbol) t d :: Constraint where
  QueueInput queue t d =
    (Ord t, KnownSymbol (InputConn queue), Schema Auto QueuedRep QueueIdQuery (Queued t d))

type family QueueOutput (queue :: Symbol) t d :: Constraint where
  QueueOutput queue t d =
    (Ord t, KnownSymbol (OutputConn queue), Schema Auto QueuedRep QueueIdQuery (Queued t d))

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
