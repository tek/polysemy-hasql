module Sqel.ReifyDd where

import Generics.SOP (I (I), K, NP (Nil, (:*)), SListI, hcollapse, tl)

import Sqel.Codec (PrimColumn (pgType))
import Sqel.Column (Nullable (Nullable))
import Sqel.Data.ColumnOptions (ColumnOptions)
import Sqel.Data.Dd (Comp, CompInc, Dd (Dd), DdK (DdK), DdStruct (DdComp, DdPrim), Struct (Comp, Prim))
import Sqel.Data.Mods (ArrayColumn (ArrayColumn), GetMod (getMod), Mods (Mods))
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.Sel (Sel (SelSymbol), SelW (SelWSymbol))
import qualified Sqel.Data.Term as Term
import Sqel.Data.Term (DdTerm (DdTerm), demoteComp, demoteInc)
import Sqel.Fold (FoldDd, foldDd)
import Sqel.SOP.Constraint (symbolText)

class ReifyPrimName a ps where
  reifyPrimName :: NP I ps -> PgPrimName

instance {-# overlappable #-} (
    ReifyPrimName a ps
  ) => ReifyPrimName a (p : ps) where
    reifyPrimName = reifyPrimName @a . tl

instance (
    PrimColumn a
  ) => ReifyPrimName a '[] where
    reifyPrimName Nil = pgType @a

instance (
    ReifyPrimName a ps
  ) => ReifyPrimName (Maybe a) (Nullable : ps) where
    reifyPrimName (I Nullable :* ps) = reifyPrimName @a ps

instance (
    ReifyPrimName a ps
  ) => ReifyPrimName (f a) (ArrayColumn f : ps) where
    reifyPrimName (I ArrayColumn :* ps) = reifyPrimName @a ps <> "[]"

instance ReifyPrimName a (PgPrimName : ps) where
  reifyPrimName (I t :* _) = t

type FoldPrim :: Sel -> Type -> Type -> Constraint
class FoldPrim sel p a where
  foldPrim :: Dd ('DdK sel p a 'Prim) -> DdTerm

instance (
    GetMod () ColumnOptions ps,
    ReifyPrimName a ps
  ) => FoldPrim ('SelSymbol name) (Mods ps) a where
    foldPrim (Dd (SelWSymbol Proxy) p@(Mods ps) DdPrim) =
      DdTerm name (Term.Prim (reifyPrimName @a ps) (getMod @() def p))
      where
        name = symbolText @name

type FoldComp :: Comp -> CompInc -> Sel -> Sel -> Type -> Type -> [DdK] -> Constraint
class FoldComp c i sel tsel p a sub where
  foldComp ::
    Dd ('DdK sel p a ('Comp tsel c i sub)) ->
    NP (K DdTerm) sub ->
    DdTerm

instance (
    SListI sub
  ) => FoldComp c i sel ('SelSymbol tname) p a sub where
    foldComp (Dd sel _ (DdComp (SelWSymbol Proxy) c i _)) sub =
      DdTerm name (Term.Comp typeName (demoteComp c) (demoteInc i) (hcollapse sub))
      where
        name = case sel of
          SelWSymbol (Proxy :: Proxy name) -> symbolText @name
          _ -> symbolText @tname
        typeName = symbolText @tname

class ReifyDd s where
  reifyDd :: Dd s -> DdTerm

instance (
    FoldDd FoldPrim FoldComp DdTerm s
  ) => ReifyDd s where
    reifyDd dd = foldDd @FoldPrim @FoldComp foldPrim foldComp dd
