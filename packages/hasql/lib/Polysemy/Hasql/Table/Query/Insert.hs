module Polysemy.Hasql.Table.Query.Insert where

import Polysemy.Hasql.Data.DbType (Column (Column), DbType (Prim, Prod, Sum))
import Polysemy.Hasql.Data.SqlCode (SqlCode (..), esql)
import Polysemy.Hasql.DbType (baseColumns)
import Polysemy.Hasql.Table.Query.Fragment (intoFragment)
import Polysemy.Hasql.Table.Query.Prepared (dollar)
import Polysemy.Hasql.Table.Query.Text (commaColumns, commaSeparated)

row :: SqlCode -> SqlCode
row a =
  [esql|row(#{a})|]

insertColumn :: Int -> Column -> (Int, SqlCode)
insertColumn index = \case
  Column _ _ _ _ Prim ->
    (index + 1, dollar index)
  Column _ _ _ _ (Prod cols) ->
    second (row . commaSeparated) (mapAccumL insertColumn index cols)
  Column _ _ _ _ (Sum cols) ->
    insertColumn index cols

insertColumns :: Int -> [Column] -> [SqlCode]
insertColumns start =
  snd . mapAccumL insertColumn start

insertValues :: Column -> [SqlCode]
insertValues = \case
  Column _ _ _ _ Prim ->
    [dollar 1]
  Column _ _ _ _ (Prod cols) ->
    insertColumns 1 cols
  Column _ _ _ _ (Sum cols) ->
    insertValues cols

insert ::
  Column ->
  SqlCode
insert table@(Column _ (intoFragment -> into) _ _ _) =
  [esql|insert #{into} (#{cols}) values (#{values})|]
  where
    cols =
      commaColumns (baseColumns table)
    values =
      commaSeparated (insertValues table)
