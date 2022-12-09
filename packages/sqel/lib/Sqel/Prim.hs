module Sqel.Prim where

import Generics.SOP (I (I), NP (Nil, (:*)))

import Sqel.Class.Mods (MapMod, SymNP, setMod, symMods)
import Sqel.Column (Nullable, nullable)
import Sqel.Data.ColumnOptions (ColumnOptions)
import Sqel.Data.Dd (ConCol, Dd (Dd), DdK (DdK), DdStruct (DdPrim), DdType, Struct (Prim))
import Sqel.Data.MigrationParams (
  MigrationDefault (MigrationDefault),
  MigrationDelete (MigrationDelete),
  MigrationRename (MigrationRename),
  )
import Sqel.Data.Mods (ArrayColumn (ArrayColumn), EnumColumn, Mods (Mods), NoMods, ReadShowColumn)
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol), SelW (SelWAuto))
import Sqel.Mods (PrimValueCodec, primEnumMods, primJsonMods, primReadShowMods)
import Sqel.Names (named)
import Sqel.SOP.Constraint (ProductGCode)

column :: Mods p -> Dd ('DdK 'SelAuto p a 'Prim)
column m =
  Dd SelWAuto m DdPrim

mods ::
  SymNP p ps =>
  p ->
  Mods ps
mods =
  symMods

primMod ::
  p ->
  Dd ('DdK 'SelAuto '[p] a 'Prim)
primMod p =
  column (Mods (I p :* Nil))

primMods ::
  SymNP p ps =>
  p ->
  Dd ('DdK 'SelAuto ps a 'Prim)
primMods p =
  column (mods p)

prim ::
  ∀ a .
  Dd ('DdK 'SelAuto NoMods a 'Prim)
prim =
  column (Mods Nil)

-- TODO move aeson to reify
json ::
  ∀ a .
  ToJSON a =>
  FromJSON a =>
  Dd ('DdK 'SelAuto [PgPrimName, PrimValueCodec a] a 'Prim)
json =
  column primJsonMods

enum ::
  ∀ a .
  Dd ('DdK 'SelAuto [PgPrimName, EnumColumn] a 'Prim)
enum =
  column primEnumMods

readShow ::
  ∀ a .
  Dd ('DdK 'SelAuto [PgPrimName, ReadShowColumn] a 'Prim)
readShow =
  column primReadShowMods

primNullable ::
  ∀ a .
  Dd ('DdK 'SelAuto [Nullable, ColumnOptions] (Maybe a) 'Prim)
primNullable =
  nullable (prim @a)

primAs ::
  ∀ name a .
  KnownSymbol name =>
  Dd ('DdK ('SelSymbol name) '[] a 'Prim)
primAs =
  named @name (prim @a)

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
  ∀ f a p sel .
  Dd ('DdK sel p a 'Prim) ->
  Dd ('DdK sel (ArrayColumn f : p) (f a) 'Prim)
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
  ) => MkPrims (a : as) ('DdK 'SelAuto '[] a 'Prim : s) where
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
