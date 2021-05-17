module Polysemy.Hasql.Table.Table where

import Polysemy.Db.Data.Column (Auto)

import Polysemy.Hasql.Column.DataColumn (DataTable, dataTable)
import Polysemy.Hasql.Tree.Table (TableRoot, tableRoot)
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.QueryRows (QueryRows(queryRows))

class GenTable rep d where
  genTable :: Table d

instance (
    TableRoot rep d tree,
    QueryRows tree d,
    QueryParams tree d,
    DataTable tree
  ) => GenTable rep d where
    genTable =
      Table (dataTable (tableRoot @rep @d)) (queryRows @tree @d) (queryParams @tree @d)

table ::
  ∀ (d :: *) .
  GenTable Auto d =>
  Table d
table = do
  genTable @Auto
