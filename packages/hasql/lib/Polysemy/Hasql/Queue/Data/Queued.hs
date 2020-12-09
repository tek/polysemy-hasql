module Polysemy.Hasql.Queue.Data.Queued where

import Polysemy.Db.Data.Column (Auto, Flatten, Prim)

data Queued t a =
  Queued {
    queue_id :: UUID,
    queue_created :: t,
    queue_payload :: a
  }
  deriving (Eq, Show, Generic)

data QueuedRep r =
  QueuedRep {
    queue_id :: Prim Auto,
    queue_created :: Prim Auto,
    queue_payload :: Flatten r
  }
  deriving (Generic)

data QueueIdQuery =
  QueueIdQuery {
    queue_id :: UUID
  }
  deriving (Eq, Show, Generic)
