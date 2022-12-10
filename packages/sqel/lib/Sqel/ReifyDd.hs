module Sqel.ReifyDd where

import Generics.SOP (I (I), K, NP (Nil, (:*)), SListI, hcollapse, tl)

import Sqel.Class.Mods (GetMod (getMod), MaybeMod (maybeMod))
import Sqel.Codec (PrimColumn (pgType))
import Sqel.Column (Nullable (Nullable))
import Sqel.Data.ColumnOptions (ColumnOptions)
import Sqel.Data.Dd (Comp, CompInc, Dd (Dd), DdK (DdK), DdStruct (DdComp, DdPrim), Struct (Comp, Prim))
import Sqel.Data.Mods (ArrayColumn (ArrayColumn), Mods (Mods), SetTableName, unSetTableName)
import Sqel.Data.PgType (PgPrimName)
import Sqel.Data.Sel (Sel (SelSymbol), SelW (SelWSymbol))
import qualified Sqel.Data.Term as Term
import Sqel.Data.Term (DdTerm (DdTerm), demoteComp, demoteInc)
import Sqel.Fold (FoldDd, foldDd)
import Sqel.SOP.Constraint (symbolText)

class ReifyPrimName a mods where
  reifyPrimName :: NP I mods -> PgPrimName

instance {-# overlappable #-} (
    ReifyPrimName a mods
  ) => ReifyPrimName a (p : mods) where
    reifyPrimName = reifyPrimName @a . tl

instance (
    PrimColumn a
  ) => ReifyPrimName a '[] where
    reifyPrimName Nil = pgType @a

instance (
    ReifyPrimName a mods
  ) => ReifyPrimName (Maybe a) (Nullable : mods) where
    reifyPrimName (I Nullable :* mods) = reifyPrimName @a mods

instance (
    ReifyPrimName a mods
  ) => ReifyPrimName (f a) (ArrayColumn f : mods) where
    reifyPrimName (I ArrayColumn :* mods) = reifyPrimName @a mods <> "[]"

instance ReifyPrimName a (PgPrimName : mods) where
  reifyPrimName (I t :* _) = t

type FoldPrim :: Sel -> [Type] -> Type -> Constraint
class FoldPrim sel p a where
  foldPrim :: Dd ('DdK sel p a 'Prim) -> DdTerm

instance (
    GetMod () ColumnOptions mods,
    MaybeMod SetTableName mods,
    ReifyPrimName a mods
  ) => FoldPrim ('SelSymbol name) mods a where
    foldPrim (Dd (SelWSymbol Proxy) mods@(Mods ms) DdPrim) =
      DdTerm name (unSetTableName <$> maybeMod mods) (Term.Prim (reifyPrimName @a ms) (getMod @() def mods))
      where
        name = symbolText @name

type FoldComp :: Comp -> CompInc -> Sel -> Sel -> [Type] -> Type -> [DdK] -> Constraint
class FoldComp c i sel tsel p a sub where
  foldComp ::
    Dd ('DdK sel p a ('Comp tsel c i sub)) ->
    NP (K DdTerm) sub ->
    DdTerm

instance (
    SListI sub,
    MaybeMod SetTableName mods
  ) => FoldComp c i sel ('SelSymbol tname) mods a sub where
    foldComp (Dd sel mods (DdComp (SelWSymbol Proxy) c i _)) sub =
      DdTerm name (unSetTableName <$> maybeMod mods) (Term.Comp typeName (demoteComp c) (demoteInc i) (hcollapse sub))
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
