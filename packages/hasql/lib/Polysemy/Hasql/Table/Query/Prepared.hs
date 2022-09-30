module Polysemy.Hasql.Table.Query.Prepared where

import Polysemy.Hasql.Data.DbType (Selector (Selector))
import Polysemy.Hasql.Data.SqlCode (SqlCode, esql)

dollar :: Int -> SqlCode
dollar i =
  [esql|$#{show i}|]

assign :: Selector -> SqlCode -> SqlCode
assign (Selector name) value =
  [esql|#{name} = #{value}|]
