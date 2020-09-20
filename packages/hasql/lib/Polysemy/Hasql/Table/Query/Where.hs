module Polysemy.Hasql.Table.Query.Where where

import qualified Data.Text as Text
import Generics.SOP.Type.Metadata (FieldInfo)

import Polysemy.Db.SOP.Constraint (RecordFields)
import qualified Polysemy.Hasql.Data.QueryWhere as Data (QueryWhere(QueryWhere))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Columns (ColumnNames(columnNames))

concatWhereFields ::
  [Text] ->
  Text
concatWhereFields =
  Text.intercalate " and " . zipWith filterField [(1 :: Int)..]
  where
    filterField index n =
      [qt|"#{n}" = $#{index}|]

where' ::
  [Text] ->
  Data.QueryWhere a query
where' fields =
  Data.QueryWhere (SqlCode (concatWhereFields fields))

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
