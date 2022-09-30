module Polysemy.Hasql.DbType where

import Polysemy.Db.Text.Quote (dquote)

import qualified Polysemy.Hasql.ColumnOptions as ColumnOptions
import qualified Polysemy.Hasql.Data.DbType as Column
import Polysemy.Hasql.Data.DbType (
  Column (Column),
  DbType (Prim, Prod, Sum),
  Name (Name),
  Selector (Selector),
  TypeName (CompositeTypeName, PrimTypeName),
  )
import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode), esql)

quotedName :: Column -> SqlCode
quotedName (Column (Name name) _ _ _ _) =
  SqlCode (dquote name)

typeName :: TypeName -> SqlCode
typeName = \case
  PrimTypeName name ->
    SqlCode name
  CompositeTypeName name ->
    [esql|ph_type__#{SqlCode name}|]

baseColumns :: Column -> [Column]
baseColumns col@(Column _ _ _ _ dbType) =
  case dbType of
    Prim -> [col]
    Prod cols -> cols
    Sum cols -> baseColumns cols

baseColumnSelectors :: Column -> [Selector]
baseColumnSelectors =
  fmap Column._selector . baseColumns

columnSpec ::
  Column ->
  SqlCode
columnSpec (Column _ (Selector selector) tpe (ColumnOptions.format -> params) _) =
  [esql|#{selector} #{typeName tpe} #{params}|]

flatColumns :: Column -> [Column]
flatColumns col@(Column _ _ _ _ dbType) =
  case dbType of
    Prim -> [col]
    Prod cols -> cols <> (flatColumns =<< cols)
    Sum c -> flatColumns c
