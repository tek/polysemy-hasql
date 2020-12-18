module Polysemy.Hasql.Table.QueryTable where

import Polysemy.Hasql.Data.QueryTable (QueryTable, QueryTable(QueryTable))
import Polysemy.Hasql.Table.Query.Where (QueryWhere(queryWhere))
import Polysemy.Hasql.Table.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.Table.Representation (Rep)
import Polysemy.Hasql.Table.Table (GenTable(genTable))

class GenQueryTable qrep rep q d where
  genQueryTable :: QueryTable q d

instance (
    GenTable rep d,
    QueryParams qrep q,
    QueryWhere rep d q
  ) =>
  GenQueryTable qrep rep q d where
    genQueryTable =
      QueryTable (genTable @rep @d) (queryParams @qrep @q) (queryWhere @rep @d @q)

queryTable ::
  ∀ q d .
  GenQueryTable (Rep q) (Rep d) q d =>
  QueryTable q d
queryTable =
  genQueryTable @(Rep q) @(Rep d)
