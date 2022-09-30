module Polysemy.Hasql.Data.Table where

import Control.Lens (makeClassy)
import Exon (exon)
import Hasql.Decoders (Row)
import Hasql.Encoders (Params)
import qualified Text.Show as Show

import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column, Name, Selector)

data Table a =
  Table {
    _structure :: Column,
    _row :: Row a,
    _params :: Params a
  }

makeClassy ''Table

name :: Lens' (Table a) Name
name =
  structure . Column.name

selector :: Lens' (Table a) Selector
selector =
  structure . Column.selector

instance Show Column => Show (Table a) where
  show (Table struct _ _) =
    [exon|Table { structure = #{show struct} }|]
