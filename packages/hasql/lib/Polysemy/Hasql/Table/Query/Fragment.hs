module Polysemy.Hasql.Table.Query.Fragment where

import Polysemy.Db.Data.ColumnOptions (ColumnOptions(ColumnOptions))
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.TableStructure (Column(Column))
import Polysemy.Db.Text.Quote (dquote)
import qualified Polysemy.Hasql.ColumnOptions as ColumnOptions
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Text (commaFields)

uniqueOrPrimary :: ColumnOptions -> Bool
uniqueOrPrimary (ColumnOptions u _ p) =
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
addFragment (Column (dquote -> name) type' (ColumnOptions.format -> params) _) =
  SqlCode [qt|add #{name} #{type'}#{params}|]

conflictFragment ::
  [Column] ->
  SqlCode ->
  SqlCode
conflictFragment columns (SqlCode setters) =
  SqlCode (format uniques)
  where
    format Nothing =
      ""
    format (Just (commaFields -> cols)) =
      [qt|on conflict (#{cols}) do update #{setters}|]
    uniques =
      nonEmpty [n | UniqueName n <- columns]
