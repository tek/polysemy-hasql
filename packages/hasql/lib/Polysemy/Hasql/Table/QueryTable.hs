module Polysemy.Hasql.Table.QueryTable where

import Polysemy.Db.Data.Column (Auto)

import Polysemy.Hasql.Data.QueryTable (QueryTable, QueryTable(QueryTable))
import Polysemy.Hasql.Where (Where(queryWhere))
import Polysemy.Hasql.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.Table.Table (GenTable(genTable))
import Polysemy.Hasql.Column.Class (TableColumn)

class GenQueryTable (qrep :: *) (rep :: *) (q :: *) (d :: *) where
  genQueryTable :: QueryTable q d

instance (
    TableColumn rep d c,
    TableColumn qrep q qc,
    GenTable rep d,
    QueryParams qc q,
    Where qc q c d
  ) =>
  GenQueryTable qrep rep q d where
    genQueryTable =
      QueryTable (genTable @rep @d) (queryParams @qc @q) (queryWhere @qc @q @c @d)

queryTable ::
  ∀ q d .
  GenQueryTable Auto Auto q d =>
  QueryTable q d
queryTable =
  genQueryTable @Auto @Auto
