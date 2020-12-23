module Polysemy.Hasql.Table.Query.Insert where

import Polysemy.Hasql.Data.SqlCode (SqlCode(..))
import Polysemy.Hasql.Table.Query.Fragment (intoFragment)
import Polysemy.Hasql.Table.Query.Prepared (dollar)
import Polysemy.Hasql.Table.Query.Text (commaColumns, commaSeparated)
import Polysemy.Hasql.Data.DbType (Column(Column), DbType(Prod, Sum, Prim))

import Polysemy.Hasql.DbType (baseColumns)

row :: Text -> Text
row a =
  [qt|row(#{a})|]

insertColumn :: Int -> Column -> (Int, Text)
insertColumn index = \case
  Column _ _ _ _ Prim ->
    (index + 1, dollar index)
  Column _ _ _ _ (Prod cols) ->
    second (row . commaSeparated) (mapAccumL insertColumn index cols)
  Column _ _ _ _ (Sum cols) ->
    second (row . commaSeparated) (mapAccumL insertColumn index cols)

insertColumns :: Int -> [Column] -> [Text]
insertColumns start =
  snd . mapAccumL insertColumn start

insertValues :: Column -> [Text]
insertValues = \case
  Column _ _ _ _ Prim ->
    [dollar 1]
  Column _ _ _ _ (Prod cols) ->
    insertColumns 1 cols
  Column _ _ _ _ (Sum cols) ->
    insertColumns 1 cols

insert ::
  Column ->
  SqlCode
insert table@(Column _ (intoFragment -> SqlCode into) _ _ _) =
  SqlCode [qt|insert #{into} (#{cols}) values (#{values})|]
  where
    cols =
      commaColumns (baseColumns table)
    values =
      commaSeparated (insertValues table)
