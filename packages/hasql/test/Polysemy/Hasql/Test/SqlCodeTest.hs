module Polysemy.Hasql.Test.SqlCodeTest where

import Polysemy.Test (UnitTest, runTestAuto, (===))

import Polysemy.Hasql.Data.SqlCode (SqlCode, esql)

test_sqlCodeNoInterpolation :: UnitTest
test_sqlCodeNoInterpolation =
  runTestAuto do
    "foo bar" === ([esql|foo bar|] :: SqlCode)
