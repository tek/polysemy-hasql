module Polysemy.Db.Data.TableStructure where

import Polysemy.Db.Data.Columns (Columns)
import Polysemy.Db.Data.TableName (TableName)

data TableStructure =
  TableStructure {
    _name :: TableName,
    _columns :: Columns
  }
  deriving (Eq, Show)

makeClassy ''TableStructure
