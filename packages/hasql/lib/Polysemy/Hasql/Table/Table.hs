module Polysemy.Hasql.Table.Table where

import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.Table.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.Table.Row (GenRow(genRow))
import Polysemy.Hasql.Table.TableStructure (GenTableStructure(genTableStructure))

class GenTable rep d where
  genTable :: Table d

instance (
    GenRow rep d,
    QueryParams rep d,
    GenTableStructure rep d
  ) =>
  GenTable rep d where
    genTable =
      Table (genTableStructure @rep @d) (genRow @rep @d) (queryParams @rep @d)
