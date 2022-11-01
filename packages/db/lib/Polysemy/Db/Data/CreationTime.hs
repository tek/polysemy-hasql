module Polysemy.Db.Data.CreationTime where

import Data.Time (UTCTime)

newtype CreationTime =
  CreationTime { unCreationTime :: UTCTime }
  deriving stock (Eq, Show, Generic)

json ''CreationTime
