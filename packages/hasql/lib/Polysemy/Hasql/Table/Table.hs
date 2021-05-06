module Polysemy.Hasql.Table.Table where

import Polysemy.Db.Data.Column (Auto)

import Polysemy.Hasql.Column.DataColumn (DataTable, dataTable)
import Polysemy.Hasql.Column.Tree (TableColumn, tableColumn)
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.QueryRows (QueryRows(queryRows))

class GenTable rep d where
  genTable :: Table d

instance (
    TableColumn rep d tree,
    QueryRows tree d,
    QueryParams tree d,
    -- PartialQueryParams tree d,
    DataTable tree
  ) => GenTable rep d where
    genTable =
      -- Table (dataTable (tableColumn @rep @d)) (queryRows @tree @d) (queryParams @tree @d) (partialQueryParams @tree @d)
      Table (dataTable (tableColumn @rep @d)) (queryRows @tree @d) (queryParams @tree @d) mempty

table ::
  ∀ (d :: *) .
  GenTable Auto d =>
  Table d
table = do
  genTable @Auto
