module Polysemy.Hasql.Queue.Data.Queue where

import GHC.TypeLits (AppendSymbol)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable)

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
      GenQueryTable QueuedRep QueueIdQuery (Queued t d)
    )

type family QueueInput (queue :: Symbol) t d :: Constraint where
  QueueInput queue t d =
    (Ord t, KnownSymbol (InputConn queue), GenQueryTable QueuedRep QueueIdQuery (Queued t d))

type family QueueOutput (queue :: Symbol) t d :: Constraint where
  QueueOutput queue t d =
    (Ord t, KnownSymbol (OutputConn queue), GenQueryTable QueuedRep QueueIdQuery (Queued t d))
