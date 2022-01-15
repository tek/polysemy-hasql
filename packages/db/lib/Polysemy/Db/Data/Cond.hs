module Polysemy.Db.Data.Cond where

newtype Less a =
  Less { unLess :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read, Fractional, IsString)

newtype LessOrEq a =
  LessOrEq { unLessOrEq :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read, Fractional, IsString)

newtype Greater a =
  Greater { unGreater :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read, Fractional, IsString)

newtype GreaterOrEq a =
  GreaterOrEq { unGreaterOrEq :: a }
  deriving (Eq, Show, Generic)
  deriving newtype (Num, Real, Enum, Integral, Ord, Read, Fractional, IsString)
