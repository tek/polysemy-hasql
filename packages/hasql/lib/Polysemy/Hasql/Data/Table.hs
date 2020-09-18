module Polysemy.Hasql.Data.Table where

import Control.Lens (Lens')
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import Polysemy.Db.Data.TableName (TableName)
import qualified Polysemy.Db.Data.TableStructure as TableStructure
import Polysemy.Db.Data.TableStructure (TableStructure)
import qualified Text.Show as Show

data Table a =
  Table {
    _structure :: TableStructure,
    _row :: Row a,
    _params :: Params a
  }
makeClassy ''Table

tableName :: Lens' (Table a) TableName
tableName =
  structure . TableStructure.name

instance Show (Table a) where
  show (Table struct _ _) =
    [qt|Table { structure = #{struct}, row = Row, params = Params }|]
