module Polysemy.Hasql.Queue.Data.Queued where

import Sqel.SOP.Constraint (symbolText)

import Sqel.Data.Dd (DbTypeName (dbTypeName))

data Queued t a =
  Queued {
    queue_created :: t,
    queue_payload :: a
  }
  deriving stock (Eq, Show, Generic)

instance (
    DbTypeName d inner,
    name ~ AppendSymbol "Queued" inner,
    KnownSymbol name
  ) => DbTypeName (Queued t d) name where
    dbTypeName =
      symbolText @name
