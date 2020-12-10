module Polysemy.Hasql.Queue.Data.Queued where

import Polysemy.Db.Data.Column (Auto, Json, Prim)

data Queued t a =
  Queued {
    queue_id :: UUID,
    queue_created :: t,
    queue_payload :: a
  }
  deriving (Eq, Show, Generic)

data QueuedRep =
  QueuedRep {
    queue_id :: Prim Auto,
    queue_created :: Prim Auto,
    queue_payload :: Json Auto
  }
  deriving (Generic)

data QueueIdQuery =
  QueueIdQuery {
    queue_id :: UUID
  }
  deriving (Eq, Show, Generic)
