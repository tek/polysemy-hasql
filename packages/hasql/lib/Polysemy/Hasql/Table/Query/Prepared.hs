module Polysemy.Hasql.Table.Query.Prepared where

import Exon (exon)

import Polysemy.Hasql.Data.DbType (Selector (Selector))
import Polysemy.Hasql.Data.SqlCode (SqlCode)

dollar :: Int -> SqlCode
dollar i =
  [exon|$#{show i}|]

assign :: Selector -> SqlCode -> SqlCode
assign (Selector name) value =
  [exon|#{name} = #{value}|]
