module Polysemy.Db.Table.Query.Select where

import Polysemy.Db.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Db.Data.TableStructure (TableStructure(..))
import Polysemy.Db.Table.Query.Fragment (fromFragment)
import Polysemy.Db.Table.Query.Text (commaColumns)

selectColumns ::
  TableStructure ->
  SqlCode
selectColumns (TableStructure (fromFragment -> SqlCode from) (commaColumns -> columns)) =
  SqlCode [i|select #{columns} #{from}|]
