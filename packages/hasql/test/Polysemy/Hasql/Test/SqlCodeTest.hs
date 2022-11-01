module Polysemy.Hasql.Test.SqlCodeTest where

import Polysemy.Test (UnitTest, runTestAuto, (===))

import Sqel.Data.Sql (Sql, sql)

test_sqlCodeNoInterpolation :: UnitTest
test_sqlCodeNoInterpolation =
  runTestAuto do
    "foo bar" === ([sql|foo bar|] :: Sql)
