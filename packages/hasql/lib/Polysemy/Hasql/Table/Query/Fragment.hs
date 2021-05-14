module Polysemy.Hasql.Table.Query.Fragment where

import Polysemy.Db.Data.ColumnOptions (ColumnOptions(ColumnOptions))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))

import Polysemy.Hasql.Table.Query.Text (commaSeparated)
import Polysemy.Hasql.Data.DbType (Column(Column), Selector(Selector))
import Polysemy.Hasql.DbType (baseColumns, columnSpec)

uniqueOrPrimary :: ColumnOptions -> Bool
uniqueOrPrimary (ColumnOptions u _ p) =
  u || p

pattern UniqueName :: Text -> Column
pattern UniqueName sel <- Column _ (Selector sel) _ (uniqueOrPrimary -> True) _

fromFragment ::
  Selector ->
  SqlCode
fromFragment (Selector name) =
  SqlCode [text|from #{name}|]

intoFragment ::
  Selector ->
  SqlCode
intoFragment (Selector name) =
  SqlCode [text|into #{name}|]

alterFragment ::
  Selector ->
  SqlCode
alterFragment (Selector name) =
  SqlCode [text|alter table #{name}|]

addColumnFragment ::
  Column ->
  SqlCode
addColumnFragment column =
  SqlCode "add " <> columnSpec column

conflictFragment ::
  Column ->
  SqlCode ->
  SqlCode
conflictFragment (baseColumns -> columns) (SqlCode setters) =
  SqlCode (format uniques)
  where
    format Nothing =
      ""
    format (Just (commaSeparated -> cols)) =
      [text|on conflict (#{cols}) do update #{setters}|]
    uniques =
      nonEmpty [n | UniqueName n <- columns]
