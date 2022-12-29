module Sqel.Product where

-- import Sqel.Comp (CompColumn, compFor)
-- import Sqel.Data.Dd (Comp (Prod), CompInc (Nest), Dd, DdK, ProdType (Reg))
-- import Sqel.Names.Rename (Rename, rename)
-- import Sqel.Names.Set (SetName)
-- import Sqel.ProductArg (ProductArg)

-- prod ::
--   ∀ (a :: Type) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
--   ProductArg a "prod" arg s0 =>
--   CompColumn ('Prod 'Reg) 'Nest a s0 s1 =>
--   arg ->
--   Dd s1
-- prod =
--   compFor @"prod" @('Prod 'Reg) @'Nest @a

-- prodAs ::
--   ∀ (name :: Symbol) (a :: Type) (s0 :: [DdK]) (s1 :: DdK) (arg :: Type) .
--   ProductArg a "prodAs" arg s0 =>
--   Rename s1 (SetName s1 name) =>
--   CompColumn ('Prod 'Reg) 'Nest a s0 s1 =>
--   arg ->
--   Dd (SetName s1 name)
-- prodAs =
--   rename . compFor @"prodAs" @('Prod 'Reg) @'Nest @a
