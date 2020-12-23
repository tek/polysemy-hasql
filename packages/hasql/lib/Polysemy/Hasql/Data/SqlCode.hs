module Polysemy.Hasql.Data.SqlCode where

import qualified Text.Show as Show

newtype SqlCode =
  SqlCode { unSqlCode :: Text }
  deriving (Eq)
  deriving newtype (IsString, Semigroup, Monoid)

instance Show SqlCode where
  show =
    toString . unSqlCode
