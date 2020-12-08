module Polysemy.Hasql.Data.DbConnection where

data DbConnection c :: Effect where
  Use :: (Int -> c -> m a) -> DbConnection c m a
  Disconnect :: DbConnection c m ()
  Reset :: DbConnection c m ()
  Info :: DbConnection c m (Text, Int)

makeSem ''DbConnection
