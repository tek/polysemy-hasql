module Sqel.Sum where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.ProductArg (ProductArg)
import Sqel.Comp (CompColumn (compColumn), compFor)
import Sqel.Data.Dd (Comp (Prod, Sum), CompInc (Merge, Nest), ConCol, Dd, DdK, ProdType (Con))
import Sqel.Names.Rename (Rename, rename)
import Sqel.Names.Set (SetName)

con ::
  ∀ (as :: [Type]) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
  ProductArg (ConCol as) "con" arg s0 =>
  CompColumn ('Prod ('Con as)) 'Nest (ConCol as) s0 s1 =>
  arg ->
  Dd s1
con =
  compFor @"con" @('Prod ('Con as)) @'Nest @(ConCol as)

conAs ::
  ∀ (sel :: Symbol) (as :: [Type]) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
  ProductArg (ConCol as) "conAs" arg s0 =>
  Rename s1 (SetName s1 sel) =>
  CompColumn ('Prod ('Con as)) 'Nest (ConCol as) s0 s1 =>
  arg ->
  Dd (SetName s1 sel)
conAs =
  rename . compFor @"conAs" @('Prod ('Con as)) @'Nest @(ConCol as)

con1 ::
  ∀ (a :: Type) (s0 :: DdK) (s1 :: DdK) .
  CompColumn ('Prod ('Con '[a])) 'Merge (ConCol '[a]) '[s0] s1 =>
  Dd s0 ->
  Dd s1
con1 dd =
  compColumn @('Prod ('Con '[a])) @'Merge @(ConCol '[a]) (dd :* Nil)

sum ::
  ∀ (a :: Type) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
  ProductArg a "sum" arg s0 =>
  CompColumn 'Sum 'Nest a s0 s1 =>
  arg ->
  Dd s1
sum =
  compFor @"sum" @'Sum @'Nest @a

sumAs ::
  ∀ (name :: Symbol) (a :: Type) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
  ProductArg a "sumAs" arg s0 =>
  CompColumn 'Sum 'Nest a s0 s1 =>
  Rename s1 (SetName s1 name) =>
  arg ->
  Dd (SetName s1 name)
sumAs =
  rename . compFor @"sumAs" @'Sum @'Nest @a
