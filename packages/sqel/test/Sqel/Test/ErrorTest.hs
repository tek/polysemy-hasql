module Sqel.Test.ErrorTest where

import qualified Control.Exception as Base
import Control.Exception (evaluate)
import qualified Data.Text as Text
import Hedgehog (TestT, (===))
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Test.Error.CompArgs (prodTooFew, prodTooMany)
import Sqel.Test.Error.NewtypeNoGeneric (newtypeNoGeneric)
import Sqel.Test.Error.NewtypeNoNewtype (newtypeNoNewtype)
import Sqel.Test.Error.QueryColumMismatch (queryColumnMismatch)

typeError ::
  Show a =>
  [Text] ->
  a ->
  TestT IO ()
typeError msg t =
  withFrozenCallStack do
    e <- liftIO $ Base.try @SomeException do
      !a <- show @Text <$> evaluate t
      pure a
    case e of
      Right _ -> fail "Test did not produce an error."
      Left err -> msg === trunc (drop 1 (lines (show err)))
  where
    trunc =
      if null msg
      then id
      else fmap Text.strip . take (length msg)

queryColumnMismatchMessage :: [Text]
queryColumnMismatchMessage =
    [
      "\8226 The query column \8216pog.p1\8217 with type \8216Int\8217 does not correspond to a table column.",
        "The specified table contains these fields:",
        "name [Text]",
        "po.p1 [Int]",
        "po.p2 [Text]",
        "id [Int64]"
    ]

prodTooFewMessage :: [Text]
prodTooFewMessage =
  [
    "\8226 The product type \8216Pr\8217 has 3 fields, but the expression specifies 1."
  ]

prodTooManyMessage :: [Text]
prodTooManyMessage =
  [
    "\8226 The product type \8216Pr\8217 has 3 fields, but the expression specifies 5."
  ]

newtypeNoGenericMessage :: [Text]
newtypeNoGenericMessage =
  [
    "\8226 \8216primNewtype\8217 declares a column for a newtype using \8216Generic\8217.",
    "The type \8216TextNt\8217 does not have an instance of \8216Generic\8217.",
    "You can add it like this:",
    "\8216newtype MyType = MyType Text deriving Generic\8217",
    "If you want to use \8216Coercible\8217 instead, use \8216primCoerce\8217."
  ]

newtypeNoNewtypeMessage :: [Text]
newtypeNoNewtypeMessage =
  [
    "\8226 \8216primNewtype\8217 declares a column for a newtype using \8216Generic\8217.",
    "The type \8216TextNt\8217 is not a newtype."
  ]

cantInferCheckQueryMessage :: [Text]
cantInferCheckQueryMessage =
  [
    -- "wha?"
  ]

test_errors :: TestTree
test_errors =
  testGroup "type errors" [
    unitTest "query column mismatch" (typeError queryColumnMismatchMessage queryColumnMismatch),
    unitTest "too few product fields" (typeError prodTooFewMessage prodTooFew),
    unitTest "too many product fields" (typeError prodTooManyMessage prodTooMany),
    unitTest "primNewtype without Generic" (typeError newtypeNoGenericMessage newtypeNoGeneric),
    unitTest "primNewtype with ADT" (typeError newtypeNoNewtypeMessage newtypeNoNewtype)
    -- ,
    -- unitTest "can't infer CheckQuery" (typeError cantInferCheckQueryMessage cantInferCheckQuery)
  ]
