module Polysemy.Hasql.Table.Query.Delete where

import Polysemy.Hasql.Data.DbType (Column (Column))
import Polysemy.Hasql.Data.SqlCode (SqlCode, esql)
import Polysemy.Hasql.Table.Query.Fragment (fromFragment)

deleteSql ::
  Column ->
  SqlCode
deleteSql (Column _ (fromFragment -> from) _ _ _) =
  [esql|delete #{from}|]
