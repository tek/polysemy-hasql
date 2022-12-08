module Sqel.Prim where

import Generics.SOP (I (I), NP (Nil, (:*)))

import Sqel.Column (MkColumn (column), Nullable, columnAs, nullable)
import Sqel.Data.ColumnOptions (ColumnOptions)
import Sqel.Data.Dd (ConCol, Dd (Dd), DdK (DdK), DdType, Struct (Prim))
import Sqel.Data.MigrationParams (
  MigrationDefault (MigrationDefault),
  MigrationDelete (MigrationDelete),
  MigrationRename (MigrationRename),
  )
import Sqel.Data.Mods (
  ArrayColumn (ArrayColumn),
  EnumColumn,
  MapMod,
  Mods (Mods),
  NoMods,
  ReadShowColumn,
  SymNP,
  setMod,
  symMods,
  )
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol))
import Sqel.Mods (PrimValueCodec, primEnumValue, primJsonValue, primReadShowValue)
import Sqel.SOP.Constraint (ProductGCode)

mods ::
  SymNP p ps =>
  p ->
  Mods ps
mods =
  symMods

primMod ::
  p ->
  Dd ('DdK 'SelAuto (Mods '[p]) a 'Prim)
primMod p =
  column (Mods (I p :* Nil))

primMods ::
  SymNP p ps =>
  p ->
  Dd ('DdK 'SelAuto (Mods ps) a 'Prim)
primMods p =
  column (mods p)

prim ::
  ∀ a .
  Dd ('DdK 'SelAuto NoMods a 'Prim)
prim =
  column (Mods Nil)

primJson ::
  ∀ a .
  ToJSON a =>
  FromJSON a =>
  Dd ('DdK 'SelAuto (Mods [PgPrimName, PrimValueCodec a]) a 'Prim)
primJson =
  column primJsonValue

enum ::
  ∀ a .
  Dd ('DdK 'SelAuto (Mods [PgPrimName, EnumColumn]) a 'Prim)
enum =
  column primEnumValue

readShow ::
  ∀ a .
  Dd ('DdK 'SelAuto (Mods [PgPrimName, ReadShowColumn]) a 'Prim)
readShow =
  column primReadShowValue

primNullable ::
  ∀ a .
  Dd ('DdK 'SelAuto (Mods [Nullable, ColumnOptions]) (Maybe a) 'Prim)
primNullable =
  nullable (prim @a)

primAs ::
  ∀ name a .
  KnownSymbol name =>
  Dd ('DdK ('SelSymbol name) NoMods a 'Prim)
primAs =
  columnAs (prim @a)

primDef ::
  ∀ s0 s1 .
  MapMod (MigrationDefault (DdType s0)) s0 s1 =>
  DdType s0 ->
  Dd s0 ->
  Dd s1
primDef a =
  setMod (MigrationDefault a)

-- TODO are composite arrays legal?
array ::
  ∀ f a p sel s .
  Dd ('DdK sel (Mods p) a s) ->
  Dd ('DdK sel (Mods (ArrayColumn f : p)) (f a) s)
array (Dd sel (Mods p) s) =
    Dd sel (Mods (I ArrayColumn :* p)) s

migrateRename ::
  ∀ name s0 s1 .
  MapMod (MigrationRename name) s0 s1 =>
  Dd s0 ->
  Dd s1
migrateRename =
  setMod (MigrationRename @name)

migrateDelete ::
  ∀ s0 s1 .
  MapMod MigrationDelete s0 s1 =>
  Dd s0 ->
  Dd s1
migrateDelete =
  setMod MigrationDelete

newtype Prims a s =
  Prims { unPrims :: NP Dd s }
  deriving stock (Generic)

class MkPrims as s | as -> s where
  mkPrims :: NP Dd s

instance MkPrims '[] '[] where
  mkPrims = Nil

instance (
    MkPrims as s
  ) => MkPrims (a : as) ('DdK 'SelAuto NoMods a 'Prim : s) where
    mkPrims = prim :* mkPrims @as @s

type family PrimProd (a :: Type) :: [Type] where
  PrimProd (ConCol as) = as
  PrimProd a = ProductGCode a

prims ::
  ∀ (a :: Type) (s :: [DdK]) .
  MkPrims (PrimProd a) s =>
  Prims a s
prims =
  Prims (mkPrims @(PrimProd a))
