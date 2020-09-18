module Polysemy.Hasql.Data.SqlCode where

newtype SqlCode =
  SqlCode { unSqlCode :: Text }
  deriving (Eq, Show)
  deriving newtype (IsString, Semigroup, Monoid)
