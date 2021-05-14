module Polysemy.Hasql.Table.Query.Prepared where

import Polysemy.Hasql.Data.DbType (Selector(Selector))

dollar :: Int -> Text
dollar i =
  [text|$#{i}|]

assign :: Selector -> Text -> Text
assign (Selector name) value =
  [text|#{name} = #{value}|]
