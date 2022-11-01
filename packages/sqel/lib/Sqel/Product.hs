module Sqel.Product where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.ProductArg (ProductArg)
import Sqel.Comp (CompColumn, compFor)
import Sqel.Data.Dd (Comp (Prod), CompInc (Merge, Nest), Dd, DdK (DdK), ProdType (Reg))
import Sqel.Data.Sel (Sel (SelAuto))
import Sqel.Data.Uid (Uid)
import Sqel.Names.Rename (Rename, rename)
import Sqel.Names.Set (SetName, SetTypeName)

prod ::
  ∀ (a :: Type) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
  ProductArg a "prod" arg s0 =>
  CompColumn ('Prod 'Reg) 'Nest a s0 s1 =>
  arg ->
  Dd s1
prod =
  compFor @"prod" @('Prod 'Reg) @'Nest @a

prodAs ::
  ∀ (name :: Symbol) (a :: Type) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
  ProductArg a "prodAs" arg s0 =>
  Rename s1 (SetName s1 name) =>
  CompColumn ('Prod 'Reg) 'Nest a s0 s1 =>
  arg ->
  Dd (SetName s1 name)
prodAs =
  rename . compFor @"prodAs" @('Prod 'Reg) @'Nest @a

merge ::
  ∀ (a :: Type) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
  ProductArg a "merge" arg s0 =>
  CompColumn ('Prod 'Reg) 'Merge a s0 s1 =>
  arg ->
  Dd s1
merge =
  compFor @"merge" @('Prod 'Reg) @'Merge @a

-- TODO do we need sa0 in the params?
type UidColumn :: Type -> Type -> DdK -> [DdK] -> DdK -> Type -> Constraint
class UidColumn i a si sa0 s arga | i a arga -> sa0, i a si sa0 -> s where
  uidColumn :: Dd si -> arga -> Dd s

instance (
    ProductArg a "merge" arga sa0,
    CompColumn ('Prod 'Reg) 'Merge a sa0 sa1,
    CompColumn ('Prod 'Reg) 'Nest (Uid i a) [si, sa1] ('DdK 'SelAuto p (Uid i a) s)
  ) => UidColumn i a si sa0 (('DdK 'SelAuto p (Uid i a) s)) arga where
    uidColumn i a =
      prod @(Uid i a) (i :* merge @a a :* Nil)

uid ::
  ∀ (i :: Type) (a :: Type)
    (si :: DdK) (sa0 :: [DdK]) (s :: DdK)
    (arga :: Type) .
  UidColumn i a si sa0 s arga =>
  Dd si ->
  arga ->
  Dd s
uid =
  uidColumn

uidAs ::
  ∀ (name :: Symbol) (i :: Type) (a :: Type)
    (si :: DdK) (sa0 :: [DdK]) (s :: DdK)
    (arga :: Type) .
  UidColumn i a si sa0 s arga =>
  Rename s (SetTypeName s name) =>
  Dd si ->
  arga ->
  Dd (SetTypeName s name)
uidAs i a =
  rename (uidColumn i a :: Dd s)
