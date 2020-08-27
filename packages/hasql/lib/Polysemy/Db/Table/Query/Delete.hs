module Polysemy.Db.Table.Query.Delete where

import Polysemy.Db.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Db.Table.Query.Fragment (fromFragment)

deleteSql ::
  TableStructure ->
  SqlCode
deleteSql (TableStructure (fromFragment -> SqlCode from) _) =
  SqlCode [i|delete #{from}|]
