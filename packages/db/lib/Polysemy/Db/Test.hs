module Polysemy.Db.Test (
  module Polysemy.Db.Test,
  module Polysemy.Db.Test.Assert,
) where

import Hedgehog (TestT, property, test, withTests)
import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

import Polysemy.Db.Test.Assert

type UnitTest = TestT IO ()

unitTest :: TestName -> UnitTest -> TestTree
unitTest desc =
  testProperty desc . withTests 1 . property . test
