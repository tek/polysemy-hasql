module Sqel.Uid where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Comp (CompColumn)
import Sqel.Data.Dd (Comp (Prod), CompInc (Nest), Dd, DdK (DdK), ProdType (Reg))
import Sqel.Data.Sel (Sel (SelAuto))
import Sqel.Data.Uid (Uid)
import Sqel.Merge (Merge, merge)
import Sqel.Names.Rename (Rename, rename)
import Sqel.Names.Set (SetTypeName)
import Sqel.Product (prod)

type UidColumn :: Type -> Type -> DdK -> DdK -> DdK -> Constraint
class UidColumn i a si sa s | i a si sa -> s where
  uidColumn :: Dd si -> Dd sa -> Dd s

instance (
    Merge a sa0 sa1,
    CompColumn ('Prod 'Reg) 'Nest (Uid i a) [si, sa1] ('DdK 'SelAuto p (Uid i a) s)
  ) => UidColumn i a si sa0 (('DdK 'SelAuto p (Uid i a) s)) where
    uidColumn i a =
      prod @(Uid i a) (i :* merge @a a :* Nil)

uid ::
  ∀ (i :: Type) (a :: Type) (si :: DdK) (sa :: DdK) (s :: DdK) .
  UidColumn i a si sa s =>
  Dd si ->
  Dd sa ->
  Dd s
uid =
  uidColumn

uidAs ::
  ∀ (name :: Symbol) (i :: Type) (a :: Type) (si :: DdK) (sa :: DdK) (s :: DdK) .
  UidColumn i a si sa s =>
  Rename s (SetTypeName s name) =>
  Dd si ->
  Dd sa ->
  Dd (SetTypeName s name)
uidAs i a =
  rename (uidColumn i a :: Dd s)
