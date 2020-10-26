module Polysemy.Hasql.Table.TableName where

import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.SOP.Constraint (DataName, dataSlug_)

class GenTableName (d :: *) where
  genTableName :: TableName

instance {-# overlappable #-} DataName d => GenTableName d where
  genTableName =
    TableName (dataSlug_ @d)

instance GenTableName d => GenTableName (Uid i d) where
  genTableName =
    genTableName @d
