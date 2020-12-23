module Polysemy.Hasql.Table.Query.Delete where

import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Fragment (fromFragment)
import Polysemy.Hasql.Data.DbType (Column(Column))

deleteSql ::
  Column ->
  SqlCode
deleteSql (Column _ (fromFragment -> SqlCode from) _ _ _) =
  SqlCode [qt|delete #{from}|]
