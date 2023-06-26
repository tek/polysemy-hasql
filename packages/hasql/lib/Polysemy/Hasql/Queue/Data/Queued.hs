module Polysemy.Hasql.Queue.Data.Queued where

data Queued t a =
  Queued {
    queue_created :: t,
    queue_payload :: a
  }
  deriving stock (Eq, Show, Generic)
