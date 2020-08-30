module Polysemy.Hasql.Data.QueryWhere where

import Polysemy.Hasql.Data.SqlCode (SqlCode)

newtype QueryWhere a q =
  QueryWhere SqlCode
  deriving (Eq, Show)
