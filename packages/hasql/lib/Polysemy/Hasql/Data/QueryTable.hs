module Polysemy.Hasql.Data.QueryTable where

import Control.Lens (Lens')
import Hasql.Encoders (Params)
import qualified Text.Show as Show

import Polysemy.Hasql.Data.DbType (Column)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Data.Table (Table)
import Polysemy.Hasql.Data.Where (Where)

-- |The full declaration of a postgres table with Hasql codecs for querying a type @d@ with a type @q@.
--
-- @
-- data User = User { id :: Int }
--
-- table :: QueryTable Int User
-- QueryTable (Table
--              (Column "user" "\"user\"" "bigint" def Prim)
--              (User <$> Hasql.Decoders.int8)
--              (User.id >$< Hasql.Encoders.int8))
--            (Hasql.Encoders.int8)
--            (Where "id = $1")
-- @
data QueryTable q d =
  QueryTable {
    _table :: Table d,
    _qparams :: Params q,
    _qwhere :: Where d q
  }

makeClassy ''QueryTable

structure :: Lens' (QueryTable q d) Column
structure =
  table . Table.structure

instance Show (QueryTable q a) where
  show (QueryTable tbl _ qw) =
    [qt|QueryTable { table = #{tbl}, qparams = Params, qwhere = #{qw} }|]
