module Polysemy.Db.Data.QueryWhere where

import Polysemy.Db.Data.SqlCode (SqlCode)

newtype QueryWhere a q =
  QueryWhere SqlCode
  deriving (Eq, Show)
