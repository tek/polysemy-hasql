module Polysemy.Hasql.Table.Query.Fragment where

import Polysemy.Db.Data.ColumnOptions (ColumnOptions (ColumnOptions))

import Polysemy.Hasql.Data.DbType (Column (Column), Selector (Selector))
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode), esql)
import Polysemy.Hasql.Data.Where (Where (Where))
import Polysemy.Hasql.DbType (baseColumns, columnSpec)
import Polysemy.Hasql.Table.Query.Select (selectColumns)
import Polysemy.Hasql.Table.Query.Text (commaSeparated)

uniqueOrPrimary :: ColumnOptions -> Bool
uniqueOrPrimary (ColumnOptions u _ p) =
  u || p

pattern UniqueName :: SqlCode -> Column
pattern UniqueName sel <- Column _ (Selector sel) _ (uniqueOrPrimary -> True) _

fromFragment ::
  Selector ->
  SqlCode
fromFragment (Selector name) =
  [esql|from #{name}|]

intoFragment ::
  Selector ->
  SqlCode
intoFragment (Selector name) =
  [esql|into #{name}|]

alterFragment ::
  Selector ->
  SqlCode
alterFragment (Selector name) =
  [esql|alter table #{name}|]

addColumnFragment ::
  Column ->
  SqlCode
addColumnFragment column =
  [esql|add #{columnSpec column}|]

conflictFragment ::
  Column ->
  SqlCode ->
  SqlCode
conflictFragment (baseColumns -> columns) setters =
  format uniques
  where
    format Nothing =
      ""
    format (Just (commaSeparated -> cols)) =
      [esql|on conflict (#{cols}) do update #{setters}|]
    uniques =
      nonEmpty [n | UniqueName n <- columns]

selectFragment ::
  Column ->
  SqlCode
selectFragment (selectColumns -> cols) =
  [esql|select #{cols}|]

whereFragment ::
  Where q d ->
  SqlCode
whereFragment = \case
  Where (SqlCode "") _ ->
    mempty
  Where qw _ ->
    [esql|where #{qw}|]
