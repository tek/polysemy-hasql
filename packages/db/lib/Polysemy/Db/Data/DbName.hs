{-# language FieldSelectors #-}

module Polysemy.Db.Data.DbName where

newtype DbName =
  DbName { unDbName :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

json ''DbName
