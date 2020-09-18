module Polysemy.Hasql.Table.Table where

import Polysemy.Hasql.Data.Table (Table(Table))
import Polysemy.Hasql.Table.QueryParams (QueryParams(queryParams))
import Polysemy.Hasql.Table.QueryRows (QueryRows(queryRows))
import Polysemy.Hasql.Table.TableStructure (GenTableStructure(genTableStructure))

class GenTable rep d where
  genTable :: Table d

instance (
    QueryRows rep d,
    QueryParams rep d,
    GenTableStructure rep d
  ) =>
  GenTable rep d where
    genTable =
      Table (genTableStructure @rep @d) (queryRows @rep @d) (queryParams @rep @d)
