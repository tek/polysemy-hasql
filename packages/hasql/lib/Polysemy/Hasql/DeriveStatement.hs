module Polysemy.Hasql.DeriveStatement where

import Hasql.Statement (Statement(Statement))
import Polysemy.Db.Data.Column (Auto)

import Polysemy.Hasql.Column.Class (TableColumn)
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.QueryParams (QueryParams, queryParams)
import Polysemy.Hasql.QueryRows (QueryRows, queryRows)
import Polysemy.Hasql.Table.ResultShape (ResultShape, resultShape)

class DeriveQuery p r where
  deriveQuery :: SqlCode -> Statement p r

instance (
    ResultShape d r,
    TableColumn Auto d dc,
    TableColumn Auto p pc,
    QueryRows dc d,
    QueryParams pc p
  ) => DeriveQuery p r where
    deriveQuery (SqlCode sql) =
      Statement (encodeUtf8 sql) (queryParams @pc @p) (resultShape (queryRows @dc @d)) True
