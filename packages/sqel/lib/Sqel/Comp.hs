{-# OPTIONS_GHC -Wno-unused-imports #-}
module Sqel.Comp where

import Generics.SOP (All, AllZip, NP (Nil, (:*)), hcmap)

import Sqel.Class.Mods (OverMod (overMod))
import Sqel.Data.Dd (
  Comp (Prod, Sum),
  CompInc (Nest),
  -- ConCoded,
  ConCol,
  DbTypeName (dbTypeName),
  Dd (Dd),
  DdInc (DdNest),
  DdK (DdK),
  DdStruct (DdComp),
  DdVar (DdProd),
  MatchDdType,
  MkDdInc (ddInc),
  MkDdVar (ddVar),
  ProdType (Con, Reg),
  Struct (Comp, Prim),
  )
import Sqel.Data.FragType (FragType (Where))
import Sqel.Data.Mods (Mods (Mods), NoMods)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol), SelW (SelWAuto), mkSel)
import Sqel.Data.Select (SelectAtom (SelectAtom))
import Sqel.Data.Sql (sql)
-- import Sqel.Names.Comp (ProdNamed, SumNamed)
-- import Sqel.Names.Rename (renameN, renameN2)
import Sqel.Prim (IndexColumn, primIndex)
import Sqel.ProductArg (ProductArg (productArg))
import Sqel.SOP.Constraint (ProductCoded)
import Sqel.Sql.Prepared (dollar)

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

-- type CompCoded :: Comp -> CompInc -> [DdK] -> Type -> [Type] -> Constraint
-- class CompCoded c i s a as | c i s a -> as where

-- instance (
--     ProductCoded a as,
--     AllZip MatchDdType s as
--   ) => CompCoded ('Prod 'Reg) i s a as where

-- -- TODO unary con
-- instance (
--     AllZip MatchDdType s as
--   ) => CompCoded ('Prod ('Con as)) i s a as where

-- instance (
--     as ~ ConCoded a,
--     AllZip MatchDdType s as
--   ) => CompCoded 'Sum i s a (Int64 : as) where

type CompName :: Type -> Sel -> Constraint
class CompName a sel | a -> sel where
  compName :: SelW sel

instance {-# overlappable #-} (
    DbTypeName a name,
    sel ~ 'SelSymbol name
  ) => CompName a sel where
    compName = dbTypeName @a

-- TODO this can now use @name@
instance CompName (ConCol name record fields as) 'SelAuto where
  compName = mkSel

-- class CompFields c a s0 s1 | c a s0 -> s1 where
--   compFields :: NP Dd s0 -> NP Dd s1

-- instance ProdNamed a s0 s1 => CompFields ('Prod 'Reg) a s0 s1 where
--   compFields = renameN

-- -- TODO overlap needed?
-- instance {-# overlappable #-} CompFields ('Prod ('Con as)) (ConCol name fields as) s0 s0 where
--   compFields = id

-- instance (
--     CompName a ('SelSymbol name),
--     KnownSymbol name,
--     SumNamed a s0 s1
--   ) => CompFields 'Sum a s0 (IndexColumn name : s1) where
--     compFields np = primIndex :* renameN2 np

-- type CompColumn' :: Comp -> CompInc -> Type -> [DdK] -> DdK -> Constraint
-- class CompColumn' c i a s0 s1 | c i a s0 -> s1 where
--   compColumn' :: NP Dd s0 -> Dd s1

-- instance (
--     MkDdVar c,
--     MkDdInc i,
--     CompName a sel,
--     CompCoded c i s0 a as,
--     CompFields c a s0 s1
--   ) => CompColumn' c i a s0 ('DdK 'SelAuto NoMods a ('Comp sel c i s1)) where
--     compColumn' np =
--       Dd SelWAuto (Mods Nil) (DdComp (compName @a) ddVar ddInc (compFields @c @a np))

-- -- | This class does nothing, but it allows downstream combinators to supply only the first three type arguments for
-- -- some reason.
-- type CompColumn :: Comp -> CompInc -> Type -> [DdK] -> DdK -> Constraint
-- class CompColumn c i a s0 s1 | c i a s0 -> s1 where
--   compColumn :: NP Dd s0 -> Dd s1

-- instance CompColumn' c i a s0 s1 => CompColumn c i a s0 s1 where
--   compColumn = compColumn' @c @i @a

-- compFor ::
--   ∀ (combi :: Symbol) (c :: Comp) (i :: CompInc) (a :: Type) (s0 :: [DdK]) (s1 :: DdK)
--     (arg :: Type) .
--   ProductArg a combi arg s0 =>
--   CompColumn c i a s0 s1 =>
--   arg ->
--   Dd s1
-- compFor =
--   compColumn @c @i @a . productArg @a @combi
