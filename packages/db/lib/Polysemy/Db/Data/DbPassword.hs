module Polysemy.Db.Data.DbPassword where

newtype DbPassword =
  DbPassword Text
  deriving (Eq, Show)
  deriving newtype (IsString)

defaultJson ''DbPassword
