module Sqel.Prim where

import Generics.SOP (I (I), NP (Nil, (:*)))

import Sqel.Column (MkColumn (column), Nullable, columnAs, nullable)
import Sqel.Data.ColumnOptions (ColumnOptions)
import Sqel.Data.Dd (ConCol, Dd, DdK (DdK), DdType, Struct (Prim))
import Sqel.Data.MigrationParams (
  MigrationDefault (MigrationDefault),
  MigrationDelete (MigrationDelete),
  MigrationRename (MigrationRename),
  )
import Sqel.Data.Mods (MapModDd, Mods (Mods), NoMods, SymNP, setMod, symMods)
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol))
import Sqel.Mods (PrimValueCodec, primJsonValue)
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
  MapModDd (MigrationDefault (DdType s0)) s0 s1 =>
  DdType s0 ->
  Dd s0 ->
  Dd s1
primDef a =
  setMod (MigrationDefault a)

migrateRename ::
  ∀ name s0 s1 .
  MapModDd (MigrationRename name) s0 s1 =>
  Dd s0 ->
  Dd s1
migrateRename =
  setMod (MigrationRename @name)

migrateDelete ::
  ∀ s0 s1 .
  MapModDd MigrationDelete s0 s1 =>
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
