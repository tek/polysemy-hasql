module Sqel.Names (
  module Sqel.Names,
  module Sqel.Names.Rename,
  module Sqel.Names.Comp,
  module Sqel.Names.Amend,
  module Sqel.Names.Set,
) where

import Sqel.Text.DbIdentifier (dbSymbol)

import Sqel.Data.Codec (ColumnName (ColumnName))
import Sqel.Data.Dd (Dd, DdK)
import Sqel.Names.Amend
import Sqel.Names.Comp
import Sqel.Names.Rename
import Sqel.Names.Set

ddName ::
  ∀ n .
  KnownSymbol n =>
  ColumnName
ddName =
  ColumnName (dbSymbol @n)

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
