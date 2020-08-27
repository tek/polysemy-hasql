module Polysemy.Db.Table.Query.Where where

import qualified Data.Text as Text
import Generics.SOP.Type.Metadata (FieldInfo)

import qualified Polysemy.Db.Data.QueryWhere as Data (QueryWhere(QueryWhere))
import Polysemy.Db.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Db.Table.Columns (ColumnNames(columnNames))
import Polysemy.Db.SOP.Constraint (RecordFields)

concatWhereFields ::
  NonEmpty Text ->
  Text
concatWhereFields =
  Text.intercalate " and " . zipWith filterField [(1 :: Int)..] . toList
  where
    filterField id' n =
      [i|"#{n}" = $#{id'}|]

where' ::
  NonEmpty Text ->
  Data.QueryWhere a query
where' fields =
  Data.QueryWhere (SqlCode [i|where #{filter'}|])
  where
    filter' =
      concatWhereFields fields

-- Construct a @where@ fragment from two types, validating that all fields of the query record and their types are
-- present and matching in the data record
class QueryWhere a query where
  queryWhere :: Data.QueryWhere a query

-- TODO re-add QueryFields
instance
  ∀ a query (qFields :: [FieldInfo]) (aFields :: [FieldInfo]) (qTypes :: [*]) (aTypes :: [*]) .
  (
  RecordFields a aFields aTypes,
  RecordFields query qFields qTypes,
  ColumnNames qFields
  -- QueryFields qFields qTypes aFields aTypes,
  ) =>
  QueryWhere a query where
    queryWhere =
      where' (columnNames @qFields)
