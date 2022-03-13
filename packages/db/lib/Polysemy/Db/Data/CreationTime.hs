module Polysemy.Db.Data.CreationTime where

import Data.Time (UTCTime)

import Polysemy.Db.Json (defaultJson)

newtype CreationTime =
  CreationTime { unCreationTime :: UTCTime }
  deriving stock (Eq, Show, Generic)

defaultJson ''CreationTime
