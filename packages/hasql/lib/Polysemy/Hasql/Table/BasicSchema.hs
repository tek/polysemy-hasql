module Polysemy.Hasql.Table.BasicSchema where

import Polysemy.Db.Data.Rep (Auto)

import Polysemy.Hasql.Table.DataColumn (DataTable, dataTable)
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.QueryRows (QueryRows(queryRows))
import Polysemy.Hasql.Tree.Table (TableRoot, tableRoot)

class BasicSchema rep d where
  basicSchema :: Table d

instance (
    TableRoot rep d tree,
    QueryRows tree d,
    QueryParams tree d,
    DataTable tree
  ) => BasicSchema rep d where
    basicSchema =
      Table (dataTable (tableRoot @rep @d)) (queryRows @tree @d) (queryParams @tree @d)

table ::
  ∀ (d :: Type) .
  BasicSchema Auto d =>
  Table d
table = do
  basicSchema @Auto
