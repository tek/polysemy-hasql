module Polysemy.Hasql.Data.QueryTable where

import Control.Lens (makeClassy)
import Exon (exon)
import Hasql.Encoders (Params)
import Polysemy.Db.Data.Uid (Uid)
import qualified Text.Show as Show

import Polysemy.Hasql.Data.DbType (Column, Name, Selector)
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
--            Hasql.Encoders.int8
--            (Where "id = $1")
-- @
data QueryTable q d =
  QueryTable {
    _table :: Table d,
    _qparams :: Params q,
    _qwhere :: Where q d
  }

makeClassy ''QueryTable

structure :: Lens' (QueryTable q d) Column
structure =
  table . Table.structure

name :: Lens' (QueryTable q d) Name
name =
  table . Table.name

selector :: Lens' (QueryTable q d) Selector
selector =
  table . Table.selector

instance Show (QueryTable q a) where
  show (QueryTable tbl _ qw) =
    [exon|QueryTable { table = #{show tbl}, qparams = Params, qwhere = #{show qw} }|]

type UidQueryTable i d =
  QueryTable i (Uid i d)
