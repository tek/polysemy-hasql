module Polysemy.Hasql.Where.Prepared where

import qualified Data.Text as Text
import Polysemy.Db.Data.Cond (Greater, GreaterOrEq, Less, LessOrEq)
import Polysemy.Db.SOP.Error (ErrorWithType2)

import Polysemy.Hasql.Table.Query.Prepared (dollar)

fieldWithOp ::
  Text ->
  Text ->
  Int ->
  Text
fieldWithOp op name index =
  [text|#{name} #{op} #{dollar index}|]

regularField ::
  Text ->
  Int ->
  Text
regularField name =
  fieldWithOp "=" name

maybeField ::
  Int ->
  Text ->
  Text
maybeField index cond =
  [text|(#{dollar index} is null or #{cond})|]

concatWhereFields ::
  [Int -> Text] ->
  Text
concatWhereFields fields =
  Text.intercalate " and " (zipWith ($) fields [(1 :: Int)..length fields])

trueField :: Int -> Text
trueField _ =
  "true"

class QueryWhereColumn q d where
  queryWhereColumn :: Text -> Int -> Text

instance {-# overlappable #-} (
  ErrorWithType2 "cannot query a column of type" d "with a query type" q
  ) => QueryWhereColumn q d where
  queryWhereColumn _ _ =
    "error"

instance {-# overlappable #-} QueryWhereColumn d d where
  queryWhereColumn =
    regularField

instance QueryWhereColumn q d => QueryWhereColumn (Maybe q) (Maybe d) where
  queryWhereColumn name i =
    maybeField i (queryWhereColumn @q @d name i)

instance {-# overlappable #-} QueryWhereColumn q d => QueryWhereColumn (Maybe q) d where
  queryWhereColumn name i =
    maybeField i (queryWhereColumn @q @d name i)

instance {-# overlappable #-} QueryWhereColumn q d => QueryWhereColumn q (Maybe d) where
  queryWhereColumn =
    queryWhereColumn @q @d

instance QueryWhereColumn (Less q) d where
  queryWhereColumn =
    fieldWithOp "<"

instance QueryWhereColumn (LessOrEq q) d where
  queryWhereColumn =
    fieldWithOp "<="

instance QueryWhereColumn (Greater q) d where
  queryWhereColumn =
    fieldWithOp ">"

instance QueryWhereColumn (GreaterOrEq q) d where
  queryWhereColumn =
    fieldWithOp ">="
