module Polysemy.Db.Data.DbEnum where

newtype DbEnum a =
  DbEnum { unDbEnum :: a }
  deriving (Eq, Show)

deriveGeneric ''DbEnum
