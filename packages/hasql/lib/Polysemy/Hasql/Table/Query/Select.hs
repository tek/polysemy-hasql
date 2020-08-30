module Polysemy.Hasql.Table.Query.Select where

import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Db.Data.TableStructure (TableStructure(..))
import Polysemy.Hasql.Table.Query.Fragment (fromFragment)
import Polysemy.Hasql.Table.Query.Text (commaColumns)

selectColumns ::
  TableStructure ->
  SqlCode
selectColumns (TableStructure (fromFragment -> SqlCode from) (commaColumns -> columns)) =
  SqlCode [i|select #{columns} #{from}|]
