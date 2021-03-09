module Polysemy.Db.Data.InitDbError where

data InitDbError =
  InitDbError Text
  deriving (Eq, Show)
