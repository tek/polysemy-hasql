module Polysemy.Hasql.Data.Table where

import Control.Lens (Lens')
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)

import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column, Name)
import qualified Text.Show as Show

data Table a =
  Table {
    _structure :: Column,
    _row :: Row a,
    _params :: Params a
  }

makeClassy ''Table

tableName :: Lens' (Table a) Name
tableName =
  structure . Column.name

instance Show Column => Show (Table a) where
  show (Table struct _ _) =
    [qt|Table { structure = #{struct}, row = Row, params = Params }|]
