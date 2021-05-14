module Polysemy.Hasql.DbType where

import Polysemy.Db.Text.Quote (dquote)

import qualified Polysemy.Hasql.ColumnOptions as ColumnOptions
import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (Column(Column), DbType(Prod, Sum, Prim), Name(Name), Selector)
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))

quotedName :: Column -> Text
quotedName (Column (Name name) _ _ _ _) =
  dquote name

baseColumns :: Column -> [Column]
baseColumns col@(Column _ _ _ _ dbType) =
  case dbType of
    Prim -> [col]
    Prod cols -> cols
    Sum cols -> baseColumns cols

baseColumnSelectors :: Column -> [Selector]
baseColumnSelectors =
  fmap Column._selector . baseColumns

baseTypes :: Column -> [Text]
baseTypes =
  fmap Column._tpe . baseColumns

columnSpec ::
  Column ->
  SqlCode
columnSpec column@(Column _ _ _ (ColumnOptions.format -> params) _) =
  SqlCode [text|#{quotedName column} #{Column._tpe column}#{params}|]

flatColumns :: Column -> [Column]
flatColumns col@(Column _ _ _ _ dbType) =
  case dbType of
    Prim -> [col]
    Prod cols -> cols <> (flatColumns =<< cols)
    Sum c -> flatColumns c
