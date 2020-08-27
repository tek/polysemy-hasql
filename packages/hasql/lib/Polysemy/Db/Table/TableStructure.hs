module Polysemy.Db.Table.TableStructure where

import Polysemy.Db.Data.Columns (Columns(Columns))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Db.Table.Columns (GenColumns(genColumns))
import Polysemy.Db.Table.TableName (GenTableName(genTableName))

class GenTableStructure d rep where
  genTableStructure :: TableStructure

instance (GenTableName d, GenColumns d rep) => GenTableStructure d rep where
  genTableStructure =
    TableStructure (genTableName @d) (Columns (genColumns @d @rep))
