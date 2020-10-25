module Polysemy.Hasql.Table.Query.Set where

import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))

import qualified Polysemy.Db.Data.TableStructure as Column
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Insert (insertValues)
import Polysemy.Hasql.Table.Query.Prepared (assign)
import Polysemy.Hasql.Table.Query.Text (commaSeparated)

set ::
  TableStructure ->
  SqlCode
set (TableStructure _ columns) =
  SqlCode [qt|set #{assigns}|]
  where
    assigns =
      commaSeparated (zipWith assign cols values)
    cols =
      (Column.columnName <$> columns)
    values =
      insertValues (toList columns)
