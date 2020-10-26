module Polysemy.Hasql.Data.QueryWhere where

import Polysemy.Hasql.Data.SqlCode (SqlCode)

newtype QueryWhere d q =
  QueryWhere SqlCode
  deriving (Eq, Show)
