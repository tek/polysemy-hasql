module Polysemy.Db.Data.InitDbError where

newtype InitDbError =
  InitDbError { unInitDbError :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)
