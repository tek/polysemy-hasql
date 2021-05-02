module Polysemy.Hasql.Table.Table where

import Polysemy.Db.Data.Column (Auto)

import Polysemy.Hasql.Column.Class (TableColumn, tableColumn)
import Polysemy.Hasql.Column.DataColumn (DataTable, dataTable)
import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.QueryParams (PartialQueryParams (partialQueryParams), QueryParams(queryParams))
import Polysemy.Hasql.QueryRows (QueryRows(queryRows))

class GenTable rep d where
  genTable :: Table d

instance (
    TableColumn rep d c,
    QueryRows c d,
    QueryParams c d,
    PartialQueryParams c d,
    DataTable c
  ) => GenTable rep d where
    genTable =
      Table (dataTable (tableColumn @rep @d)) (queryRows @c @d) (queryParams @c @d) (partialQueryParams @c @d)

table ::
  ∀ (d :: *) .
  GenTable Auto d =>
  Table d
table = do
  genTable @Auto
