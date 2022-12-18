module Sqel.Data.FragType where

import Sqel.Data.Order (Order)
import Sqel.Data.Sql (Sql, sql)

data FragType =
  Where
  |
  Offset
  |
  Limit
  |
  Order Order
  |
  Custom Int Text
  deriving stock (Eq, Show, Generic)

renderWithFragKeyword :: Sql -> FragType -> Sql
renderWithFragKeyword param = \case
  Where -> [sql|where #{param}|]
  Offset -> [sql|offset #{param}|]
  Limit -> [sql|limit #{param}|]
  Order dir -> [sql|order by #{param} ##{dir}|]
  Custom _ kw -> [sql|##{kw} #{param}|]

sfragOrdinal :: FragType -> Int
sfragOrdinal = \case
  Where -> 0
  Order _ -> 1
  Limit -> 2
  Offset -> 3
  Custom i _ -> i

instance Ord FragType where
  compare =
    comparing sfragOrdinal
