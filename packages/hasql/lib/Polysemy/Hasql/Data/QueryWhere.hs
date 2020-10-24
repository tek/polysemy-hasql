module Polysemy.Hasql.Data.QueryWhere where

import Polysemy.Hasql.Data.SqlCode (SqlCode)

newtype Less a =
  Less { unLess :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read, Fractional)

newtype LessOrEq a =
  LessOrEq { unLessOrEq :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read, Fractional)

newtype Greater a =
  Greater { unGreater :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read, Fractional)

newtype GreaterOrEq a =
  GreaterOrEq { unGreaterOrEq :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read, Fractional)

newtype QueryWhere a q =
  QueryWhere SqlCode
  deriving (Eq, Show)
