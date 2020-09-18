module Polysemy.Hasql.Data.QueryTable where

import Control.Lens (Lens')
import Hasql.Encoders (Params)

import Polysemy.Hasql.Data.QueryWhere (QueryWhere)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Data.Table (Table)
import Polysemy.Db.Data.TableStructure (TableStructure)
import qualified Text.Show as Show

data QueryTable q a =
  QueryTable {
    _table :: Table a,
    _qparams :: Params q,
    _qwhere :: QueryWhere a q
  }

makeClassy ''QueryTable

structure :: Lens' (QueryTable q a) TableStructure
structure =
  table . Table.structure

instance Show (QueryTable q a) where
  show (QueryTable tbl _ qw) =
    [qt|QueryTable { table = #{tbl}, qparams = Params, qwhere = #{qw} }|]
