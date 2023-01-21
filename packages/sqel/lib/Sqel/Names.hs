module Sqel.Names (
  module Sqel.Names,
  module Sqel.Names.Rename,
  module Sqel.Names.Set,
) where

import Sqel.Data.Codec (ColumnName (ColumnName))
import Sqel.Data.Dd (Dd, DdK)
import Sqel.Data.Sel (Sel)
import Sqel.Names.Rename
import Sqel.Names.Set
import Sqel.Text.DbIdentifier (dbSymbol)

ddName ::
  ∀ n .
  KnownSymbol n =>
  ColumnName
ddName =
  ColumnName (dbSymbol @n)

selAs ::
  ∀ (sel :: Sel) (s0 :: DdK) .
  Rename s0 (SetSel s0 sel) =>
  Dd s0 ->
  Dd (SetSel s0 sel)
selAs =
  rename

named ::
  ∀ (name :: Symbol) (s0 :: DdK) .
  Rename s0 (SetName s0 name) =>
  Dd s0 ->
  Dd (SetName s0 name)
named =
  rename

typeAs ::
  ∀ (name :: Symbol) (s0 :: DdK) .
  Rename2 s0 (SetTypeName s0 name) =>
  Dd s0 ->
  Dd (SetTypeName s0 name)
typeAs =
  rename2
