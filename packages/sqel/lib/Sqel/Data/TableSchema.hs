module Sqel.Data.TableSchema where

import Exon (exon)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import Text.Show (showParen, showsPrec)

import Sqel.Data.PgType (PgTable)

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
