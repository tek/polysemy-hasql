module Polysemy.Db.Data.StoreError where

data StoreError a =
  Backend a
  deriving (Eq, Show)
