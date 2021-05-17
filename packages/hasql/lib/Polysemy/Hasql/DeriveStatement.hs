module Polysemy.Hasql.DeriveStatement where

import Hasql.Statement (Statement(Statement))
import Polysemy.Db.Data.Column (Auto)

import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.QueryParams (QueryParams, queryParams)
import Polysemy.Hasql.QueryRows (QueryRows, queryRows)
import Polysemy.Hasql.Table.ResultShape (ResultShape, resultShape)
import Polysemy.Hasql.Tree.Table (TableRoot)

class DeriveQuery p r where
  deriveQuery :: SqlCode -> Statement p r

instance (
    ResultShape d r,
    TableRoot Auto d dc,
    TableRoot Auto p pc,
    QueryRows dc d,
    QueryParams pc p
  ) => DeriveQuery p r where
    deriveQuery (SqlCode sql) =
      Statement (encodeUtf8 sql) (queryParams @pc @p) (resultShape (queryRows @dc @d)) True
