module Polysemy.Hasql.Data.Table where

import Control.Lens (Lens')
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import Polysemy.Db.Data.PartialFields (PartialFields)
import qualified Text.Show as Show

import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column, Name, Selector)

data Table a =
  Table {
    _structure :: Column,
    _row :: Row a,
    _params :: Params a,
    _partialParams :: Params (PartialFields a)
  }

makeClassy ''Table

name :: Lens' (Table a) Name
name =
  structure . Column.name

selector :: Lens' (Table a) Selector
selector =
  structure . Column.selector

instance Show Column => Show (Table a) where
  show (Table struct _ _ _) =
    [qt|Table { structure = #{struct} }|]
