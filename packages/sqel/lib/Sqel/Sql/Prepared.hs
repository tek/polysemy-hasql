module Sqel.Sql.Prepared where

import Sqel.Data.Sql (Sql, sql)

dollar :: Int -> Sql
dollar i =
  [sql|$#{show i}|]
