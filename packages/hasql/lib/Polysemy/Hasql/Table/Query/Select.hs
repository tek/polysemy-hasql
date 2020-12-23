module Polysemy.Hasql.Table.Query.Select where

import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Fragment (fromFragment)
import Polysemy.Hasql.Table.Query.Text (commaSeparated)
import Polysemy.Hasql.Data.DbType (Column(Column), DbType(Sum, Prim, Prod))

import Polysemy.Hasql.Data.DbType (Selector(Selector))

foldPrims ::
  Monoid a =>
  (Text -> a) ->
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
        foldMap spin cols

selectColumns ::
  Column ->
  SqlCode
selectColumns column@(Column _ (fromFragment -> SqlCode from) _ _ _) =
  SqlCode [qt|select #{commaSeparated @[] (foldPrims pure column)} #{from}|]
