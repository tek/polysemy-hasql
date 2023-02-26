module Sqel.Data.Projection where

import Exon (exon)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (PgTable (PgTable))
import Sqel.Data.ProjectionWitness (ProjectionWitness)
import Sqel.Data.Sql (ToSql (toSql), sql)
import Sqel.Data.SqlFragment (From (From), Select (Select))
import qualified Sqel.Data.TableSchema as TableSchema
import Sqel.Data.TableSchema (TableSchema (TableSchema))

data Projection proj table =
  Projection {
    pg :: PgTable proj,
    decoder :: Row proj,
    encoder :: Params proj,
    table :: TableSchema table,
    witness :: ProjectionWitness proj table
  }
  deriving stock (Generic)

instance Show (Projection proj table) where
  showsPrec d Projection {pg} =
    showParen (d > 10) [exon|Projection #{showsPrec 11 pg}|]

instance ToSql (Select (Projection proj table)) where
  toSql (Select Projection {pg = PgTable {selectors}, table = TableSchema {pg = PgTable {name}}}) =
    [sql|##{Select selectors} ##{From name}|]
