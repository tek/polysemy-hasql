module Polysemy.Hasql.Data.QueryTable where

import Hasql.Encoders (Params)

import Control.Lens (Lens')
import Polysemy.Hasql.Data.Where (Where)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Data.Table (Table)
import Polysemy.Hasql.Data.DbType (Column)
import qualified Text.Show as Show

data QueryTable q a =
  QueryTable {
    _table :: Table a,
    _qparams :: Params q,
    _qwhere :: Where a q
  }

makeClassy ''QueryTable

structure :: Lens' (QueryTable q a) Column
structure =
  table . Table.structure

instance Show (QueryTable q a) where
  show (QueryTable tbl _ qw) =
    [qt|QueryTable { table = #{tbl}, qparams = Params, qwhere = #{qw} }|]
