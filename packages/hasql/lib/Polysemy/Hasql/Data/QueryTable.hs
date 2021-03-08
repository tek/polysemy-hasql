module Polysemy.Hasql.Data.QueryTable where

import Hasql.Encoders (Params)

import Control.Lens (Lens')
import Polysemy.Hasql.Data.DbType (Column)
import qualified Polysemy.Hasql.Data.Table as Table
import Polysemy.Hasql.Data.Table (Table)
import Polysemy.Hasql.Data.Where (Where)
import qualified Text.Show as Show

-- |The full declaration of a postgres table with Hasql codecs for querying a type @a@ with a type @q@.
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
