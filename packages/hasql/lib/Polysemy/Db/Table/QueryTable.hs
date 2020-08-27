module Polysemy.Db.Table.QueryTable where

import Polysemy.Db.Data.QueryTable (QueryTable, QueryTable(QueryTable))
import Polysemy.Db.Table.Query.Where (QueryWhere(queryWhere))
import Polysemy.Db.Table.QueryParams (QueryParams(queryParams))
import Polysemy.Db.Table.Table (GenTable(genTable))

class GenQueryTable d rep q where
  genQueryTable :: QueryTable d q

instance (
    GenTable d rep,
    QueryParams q q,
    QueryWhere d q
  ) =>
  GenQueryTable d rep q where
    genQueryTable =
      QueryTable (genTable @d @rep) (queryParams @q @q) queryWhere
