module Polysemy.Db.Data.QueryTable where

import Hasql.Encoders (Params)

import Polysemy.Db.Data.QueryWhere (QueryWhere)
import Polysemy.Db.Data.Table (Table)
import qualified Text.Show as Show

data QueryTable a q =
  QueryTable {
    _table :: Table a,
    _qparams :: Params q,
    _qwhere :: QueryWhere a q
  }

makeClassy ''QueryTable

instance Show (QueryTable a q) where
  show (QueryTable tbl _ qw) =
    [i|QueryTable { table = #{tbl}, qparams = Params, qwhere = #{qw} }|]
