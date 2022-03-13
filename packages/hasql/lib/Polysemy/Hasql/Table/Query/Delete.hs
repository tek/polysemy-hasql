module Polysemy.Hasql.Table.Query.Delete where

import Exon (exon)
import Polysemy.Hasql.Data.DbType (Column (Column))
import Polysemy.Hasql.Data.SqlCode (SqlCode)
import Polysemy.Hasql.Table.Query.Fragment (fromFragment)

deleteSql ::
  Column ->
  SqlCode
deleteSql (Column _ (fromFragment -> from) _ _ _) =
  [exon|delete #{from}|]
