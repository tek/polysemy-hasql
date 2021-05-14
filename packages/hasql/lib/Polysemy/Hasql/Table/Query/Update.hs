module Polysemy.Hasql.Table.Query.Update where

import Control.Lens (view)

import Polysemy.Hasql.Data.DbType (Selector(Selector))
import Polysemy.Hasql.Data.QueryTable (QueryTable, selector)
import Polysemy.Hasql.Data.SqlCode (SqlCode(..))

update ::
  QueryTable query d ->
  SqlCode
update (view selector -> Selector name) =
  SqlCode [text|update #{name}|]
