module Polysemy.Hasql.Table.Query.Select where

import Polysemy.Hasql.Data.DbType (Column (Column), DbType (Prim, Prod, Sum), Selector (Selector))
import Polysemy.Hasql.Data.SqlCode (SqlCode)
import Polysemy.Hasql.Table.Query.Text (commaSeparated)

foldPrims ::
  Monoid a =>
  (SqlCode -> a) ->
  Column ->
  a
foldPrims prim =
  spin
  where
    spin = \case
      Column _ (Selector selector) _ _ Prim ->
        prim selector
      Column _ _ _ _ (Prod cols) ->
        foldMap spin cols
      Column _ _ _ _ (Sum cols) ->
        spin cols

selectColumns ::
  Column ->
  SqlCode
selectColumns column =
  commaSeparated @[] (foldPrims pure column)
