module Polysemy.Hasql.Queue.Data.Queued where

import GHC.TypeLits (AppendSymbol)
import Polysemy.Db.Data.Column (Json, Prim)

import Polysemy.Hasql.Column.Class (TableName)

data Queued t a =
  Queued {
    queue_id :: UUID,
    queue_created :: t,
    queue_payload :: a
  }
  deriving (Eq, Show, Generic)

instance (
    TableName d inner,
    name ~ AppendSymbol "Queued" inner,
    KnownSymbol name
  ) => TableName (Queued t d) name where

data QueuedRep =
  QueuedRep {
    queue_id :: Prim,
    queue_created :: Prim,
    queue_payload :: Json
  }
  deriving (Generic)

data QueueIdQuery =
  QueueIdQuery {
    queue_id :: UUID
  }
  deriving (Eq, Show, Generic)

data QueueIdQueryRep =
  QueueIdQueryRep {
    queue_id :: Prim
  }
  deriving (Eq, Show, Generic)
