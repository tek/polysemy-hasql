module Polysemy.Db.Table.Table where

import Polysemy.Db.Data.Table (Table(Table))
import Polysemy.Db.Table.QueryParams (QueryParams(queryParams))
import Polysemy.Db.Table.Row (GenRow(genRow))
import Polysemy.Db.Table.TableStructure (GenTableStructure(genTableStructure))

class GenTable d rep where
  genTable :: Table d

instance (
    GenRow d rep,
    QueryParams d rep,
    GenTableStructure d rep
  ) =>
  GenTable d rep where
    genTable =
      Table (genTableStructure @d @rep) (genRow @d @rep) (queryParams @d @rep)
