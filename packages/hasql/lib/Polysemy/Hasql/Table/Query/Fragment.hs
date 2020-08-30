module Polysemy.Hasql.Table.Query.Fragment where

import qualified Polysemy.Hasql.ColumnParams as ColumnParams
import Polysemy.Db.Data.ColumnParams (ColumnParams(ColumnParams))
import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Hasql.Table.Query.Text (commaFields)
import Polysemy.Db.Text.Quote (dquote)

uniqueOrPrimary :: ColumnParams -> Bool
uniqueOrPrimary (ColumnParams u _ p) =
  u || p

pattern UniqueName :: Text -> Column
pattern UniqueName name <- Column name _ (uniqueOrPrimary -> True)

fromFragment ::
  TableName ->
  SqlCode
fromFragment (TableName (dquote -> name)) =
  SqlCode [i|from #{name}|]

intoFragment ::
  TableName ->
  SqlCode
intoFragment (TableName (dquote -> name)) =
  SqlCode [i|into #{name}|]

alterFragment ::
  TableName ->
  SqlCode
alterFragment (TableName (dquote -> name)) =
  SqlCode [i|alter table #{name}|]

addFragment ::
  Column ->
  SqlCode
addFragment (Column (dquote -> name) type' (ColumnParams.format -> params)) =
  SqlCode [i|add #{name} #{type'}#{params}|]

conflictFragment ::
  Columns ->
  Text ->
  SqlCode
conflictFragment (Columns columns) setters =
  SqlCode (format uniques)
  where
    format Nothing =
      ""
    format (Just (commaFields -> cols)) =
      [i|on conflict (#{cols}) do update #{setters}|]
    uniques =
      nonEmpty [n | UniqueName n <- toList columns]
