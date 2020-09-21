module Polysemy.Hasql.Table.TableStructure where

import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
import Polysemy.Hasql.Table.Columns (Columns(columns))
import Polysemy.Hasql.Table.Representation (Rep)
import Polysemy.Hasql.Table.TableName (GenTableName(genTableName))

class GenTableStructure rep d where
  genTableStructure :: TableStructure

instance (GenTableName d, Columns rep d) => GenTableStructure rep d where
  genTableStructure =
    TableStructure (genTableName @d) (columns @rep @d)

tableStructure ::
  ∀ (d :: *) .
  GenTableStructure (Rep d) d =>
  TableStructure
tableStructure =
  genTableStructure @(Rep d) @d
