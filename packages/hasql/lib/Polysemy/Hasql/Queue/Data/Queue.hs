module Polysemy.Hasql.Queue.Data.Queue where

import Polysemy.Hasql.Table.QueryTable (GenQueryTable)

import Polysemy.Hasql.Queue.Data.Queued (QueueIdQuery, Queued, QueuedRep)

type family Queue (queue :: Symbol) t d :: Constraint where
  Queue queue t d =
    (Ord t, KnownSymbol queue, GenQueryTable QueuedRep QueueIdQuery (Queued t d))
