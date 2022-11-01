module Sqel.Sql.CommaSep where

import qualified Exon

import Sqel.Data.Sql (ToSql (toSql))

newtype CommaSep a =
  CommaSep { unReturning :: a }
  deriving stock (Eq, Show)

instance ToSql a => ToSql (CommaSep [a]) where
  toSql (CommaSep a) =
    Exon.intercalate ", " (toSql <$> a)
