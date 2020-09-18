module Polysemy.Hasql.Table.Query.Fragment where

import qualified Polysemy.Hasql.ColumnParams as ColumnParams
import Polysemy.Db.Data.ColumnParams (ColumnParams(ColumnParams))
import Polysemy.Db.Data.TableStructure (Column(Column))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Hasql.Table.Query.Text (commaFields)
import Polysemy.Db.Text.Quote (dquote)

uniqueOrPrimary :: ColumnParams -> Bool
uniqueOrPrimary (ColumnParams u _ p) =
  u || p

pattern UniqueName :: Text -> Column
pattern UniqueName name <- Column name _ (uniqueOrPrimary -> True) _

fromFragment ::
  TableName ->
  SqlCode
fromFragment (TableName (dquote -> name)) =
  SqlCode [qt|from #{name}|]

intoFragment ::
  TableName ->
  SqlCode
intoFragment (TableName (dquote -> name)) =
  SqlCode [qt|into #{name}|]

alterFragment ::
  TableName ->
  SqlCode
alterFragment (TableName (dquote -> name)) =
  SqlCode [qt|alter table #{name}|]

addFragment ::
  Column ->
  SqlCode
addFragment (Column (dquote -> name) type' (ColumnParams.format -> params) _) =
  SqlCode [qt|add #{name} #{type'}#{params}|]

conflictFragment ::
  NonEmpty Column ->
  Text ->
  SqlCode
conflictFragment columns setters =
  SqlCode (format uniques)
  where
    format Nothing =
      ""
    format (Just (commaFields -> cols)) =
      [qt|on conflict (#{cols}) do update #{setters}|]
    uniques =
      nonEmpty [n | UniqueName n <- toList columns]
