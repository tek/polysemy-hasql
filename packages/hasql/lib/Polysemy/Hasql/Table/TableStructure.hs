module Polysemy.Hasql.Table.TableStructure where

import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Hasql.Table.Columns (GenColumns(genColumns))
import Polysemy.Hasql.Table.TableName (GenTableName(genTableName))

class GenTableStructure rep d where
  genTableStructure :: TableStructure

instance (GenTableName d, GenColumns rep d) => GenTableStructure rep d where
  genTableStructure =
    TableStructure (genTableName @d) (genColumns @rep @d)
