module Sqel.Test.SqlCodeTest where

import Hedgehog ((===), TestT)

import Sqel.Data.Sql (Sql, sql)

test_sqlCodeNoInterpolation :: TestT IO ()
test_sqlCodeNoInterpolation =
  "foo bar" === ([sql|foo bar|] :: Sql)
