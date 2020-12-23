module Polysemy.Hasql.Data.Where where

import Polysemy.Hasql.Data.SqlCode (SqlCode)

newtype Where d q =
  Where SqlCode
  deriving (Eq, Show)
