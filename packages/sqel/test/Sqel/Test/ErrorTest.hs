module Sqel.Test.ErrorTest where

import qualified Control.Exception as Base
import Control.Exception (evaluate)
import qualified Data.Text as Text
import Hedgehog (TestT, (===))
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Test.Error.CompArgs (prodTooFew)
import Sqel.Test.Error.QueryColumMismatch (queryColumnMismatch)

typeError ::
  [Text] ->
  a ->
  TestT IO ()
typeError msg t = do
  e <- liftIO (Base.try @SomeException (evaluate t))
  case e of
    Right _ -> unit
    Left err -> msg === (trunc (drop 1 (lines (show err))))
  where
    trunc =
      if null msg
      then id
      else fmap Text.strip . take (length msg)

queryColumnMismatchMessage :: [Text]
queryColumnMismatchMessage =
    ["\8226 The query column \8216pog.p1\8217 with type \8216Int\8217 does not correspond to a table column."]

prodTooFewMessage :: [Text]
prodTooFewMessage =
  [
    "\8226 The product type \8216Pr\8217 has 3 fields, but the expression specifies 1."
  ]

test_errors :: TestTree
test_errors =
  testGroup "type errors" [
    unitTest "query column mismatch" (typeError queryColumnMismatchMessage queryColumnMismatch),
    unitTest "too few product fields" (typeError prodTooFewMessage prodTooFew)
  ]
