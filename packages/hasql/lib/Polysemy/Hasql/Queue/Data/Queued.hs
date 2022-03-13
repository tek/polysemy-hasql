module Polysemy.Hasql.Queue.Data.Queued where

import Polysemy.Db.Data.Rep (Json, Prim)
import Polysemy.Db.Tree (RootName)

data Queued t a =
  Queued {
    queue_created :: t,
    queue_payload :: a
  }
  deriving stock (Eq, Show, Generic)

instance (
    RootName d inner,
    name ~ AppendSymbol "Queued" inner,
    KnownSymbol name
  ) => RootName (Queued t d) name where

data QueuedRep =
  QueuedRep {
    queue_created :: Prim,
    queue_payload :: Json
  }
  deriving stock (Generic)
