module Polysemy.Hasql.Where.Prepared where

import qualified Exon
import Exon (exon)
import Polysemy.Db.Data.Cond (Greater, GreaterOrEq, Less, LessOrEq)
import Polysemy.Db.SOP.Error (ErrorWithType2)

import Polysemy.Hasql.Data.SqlCode (SqlCode)
import Polysemy.Hasql.Table.Query.Prepared (dollar)

fieldWithOp ::
  SqlCode ->
  SqlCode ->
  Int ->
  SqlCode
fieldWithOp op name index =
  [exon|#{name} #{op} #{dollar index}|]

regularField ::
  SqlCode ->
  Int ->
  SqlCode
regularField name =
  fieldWithOp "=" name

maybeField ::
  Int ->
  SqlCode ->
  SqlCode
maybeField index cond =
  [exon|(#{dollar index} is null or #{cond})|]

concatWhereFields ::
  [Int -> SqlCode] ->
  SqlCode
concatWhereFields fields =
  Exon.intercalate " and " (zipWith ($) fields [(1 :: Int)..length fields])

trueField :: Int -> Text
trueField _ =
  "true"

class QueryWhereColumn q d where
  queryWhereColumn :: SqlCode -> Int -> SqlCode

instance {-# overlappable #-} (
  ErrorWithType2 "Cannot query a column of type" d "with a query type" q
  ) => QueryWhereColumn q d where
  queryWhereColumn _ _ =
    "error"

instance {-# overlappable #-} QueryWhereColumn d d where
  queryWhereColumn =
    regularField

instance (
    QueryWhereColumn q d
  ) => QueryWhereColumn (Maybe q) (Maybe d) where
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
