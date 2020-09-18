module Polysemy.Hasql.Table.Query.Delete where

import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Hasql.Table.Query.Fragment (fromFragment)

deleteSql ::
  TableStructure ->
  SqlCode
deleteSql (TableStructure (fromFragment -> SqlCode from) _) =
  SqlCode [qt|delete #{from}|]
