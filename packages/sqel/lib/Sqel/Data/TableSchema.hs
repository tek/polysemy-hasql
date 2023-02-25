module Sqel.Data.TableSchema where

import Exon (exon)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Sqel.Data.Create (Create (Create))
import Sqel.Data.PgType (PgTable)
import Sqel.Data.SqlFragment (Select (Select))
import Sqel.Data.Sql (ToSql (toSql))

data TableSchema a =
  TableSchema {
    pg :: PgTable a,
    decoder :: Row a,
    encoder :: Params a
  }
  deriving stock (Generic)

instance Show (TableSchema a) where
  showsPrec d TableSchema {pg} =
    showParen (d > 10) [exon|TableSchema #{showsPrec 11 pg}|]

instance ToSql (Select (TableSchema a)) where
  toSql (Select ts) =
    toSql (Select (ts ^. #pg))

instance ToSql (Create (TableSchema a)) where
  toSql (Create ts) =
    toSql (Create (ts ^. #pg))
