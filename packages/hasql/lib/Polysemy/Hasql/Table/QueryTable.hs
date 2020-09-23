module Polysemy.Hasql.Table.QueryTable where

import Polysemy.Hasql.Data.QueryTable (QueryTable, QueryTable(QueryTable))
import Polysemy.Hasql.Table.Query.Where (QueryWhere(queryWhere))
import Polysemy.Hasql.Table.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.Table.Representation (Rep)
import Polysemy.Hasql.Table.Table (GenTable(genTable))

class GenQueryTable rep q d where
  genQueryTable :: QueryTable q d

instance (
    GenTable rep d,
    QueryParams (Rep q) q,
    QueryWhere d q
  ) =>
  GenQueryTable rep q d where
    genQueryTable =
      QueryTable (genTable @rep @d) (queryParams @(Rep q) @q) queryWhere

queryTable ::
  ∀ q d .
  GenQueryTable (Rep d) q d =>
  QueryTable q d
queryTable =
  genQueryTable @(Rep d)
