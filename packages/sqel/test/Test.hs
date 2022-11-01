module Test where

import Hedgehog (TestT, property, test, withTests)

import Test.Tasty (TestName, TestTree)
import Test.Tasty.Hedgehog (testProperty)

unitTest ::
  TestName ->
  TestT IO () ->
  TestTree
unitTest desc =
  testProperty desc . withTests 1 . property . test
