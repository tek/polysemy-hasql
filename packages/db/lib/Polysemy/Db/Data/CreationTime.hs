module Polysemy.Db.Data.CreationTime where

import Data.Time (UTCTime)

newtype CreationTime =
  CreationTime { unCreationTime :: UTCTime }
  deriving (Eq, Show, Generic)

defaultJson ''CreationTime
