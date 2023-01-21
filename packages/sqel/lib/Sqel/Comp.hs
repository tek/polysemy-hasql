module Sqel.Comp where

import Generics.SOP.GGP (GDatatypeInfoOf)

import Sqel.Data.Sel (MkSel, Sel (SelType), SelPrefix (DefaultPrefix), SelW, mkSel)
import Sqel.Data.Uid (Uid)
import Sqel.SOP.Constraint (IsDataT)

-- TODO reimplement for new class structure
-- -- TODO this recurses through the entire subtree.
-- -- can we replace this with a check for the index column? i.e. ($1 != 2 or foo = $2)
-- -- TODO this has to be moved to the post builder
-- class GuardSumPrim s where
--   guardSumPrim :: Dd s -> Dd s

-- instance (
--     OverMod SelectAtom ('DdK sel p a 'Prim)
--   ) => GuardSumPrim ('DdK sel p a 'Prim) where
--   guardSumPrim =
--     overMod \case
--       SelectAtom Where code ->
--         SelectAtom Where (\ s i -> [sql|(#{dollar i} is null or #{code s i})|])
--       m -> m

-- instance (
--     All GuardSumPrim sub
--   ) => GuardSumPrim ('DdK sel param a ('Comp tsel ('Prod 'Reg) 'Nest sub)) where
--     guardSumPrim (Dd sel p (DdComp tsel DdProd DdNest sub)) =
--       Dd sel p (DdComp tsel DdProd DdNest (hcmap (Proxy @GuardSumPrim) guardSumPrim sub))

type CompName :: Type -> Sel -> Constraint
class CompName a sel | a -> sel where
  compName :: SelW sel

instance {-# overlappable #-} (
    IsDataT (GDatatypeInfoOf a) name,
    sel ~ 'SelType 'DefaultPrefix name,
    MkSel sel
  ) => CompName a sel where
    compName = mkSel

instance CompName a sel => CompName (Uid i a) sel where
  compName = compName @a

-- -- TODO this can now use @name@
-- instance CompName (ConCol name record fields as) 'SelAuto where
--   compName = mkSel
