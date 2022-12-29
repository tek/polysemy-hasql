module Sqel.ReifyDd where

import Generics.SOP (All, I (I), NP (Nil, (:*)), hcfoldMap, tl)

import Sqel.Class.Mods (MaybeMod (maybeMod))
import Sqel.Codec (PrimColumn (pgType))
import Sqel.ColumnConstraints (ColumnConstraints, columnConstraints)
import Sqel.Data.Dd (Dd (Dd), DdK (DdK), DdStruct (DdComp, DdPrim), Struct (Comp, Prim))
import Sqel.Data.Mods (
  ArrayColumn (ArrayColumn),
  Mods (Mods),
  Newtype (Newtype),
  Nullable (Nullable),
  SetTableName,
  unSetTableName,
  )
import Sqel.Data.PgType (PgPrimName, pgColumnName)
import Sqel.Data.Sel (ReifySel (reifySel), Sel (SelSymbol), SelW (SelWSymbol))
import qualified Sqel.Data.Term as Term
import Sqel.Data.Term (DdTerm (DdTerm), demoteComp, demoteInc)
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

instance (
    ReifyPrimName w mods
  ) => ReifyPrimName a (Newtype a w : mods) where
    reifyPrimName (I (Newtype _ _) :* mods) = reifyPrimName @w mods

instance ReifyPrimName a (PgPrimName : mods) where
  reifyPrimName (I t :* _) = t

class ReifyDd s where
  reifyDd :: Dd s -> DdTerm

instance (
    ColumnConstraints mods,
    MaybeMod SetTableName mods,
    ReifyPrimName a mods,
    ReifySel sel
  ) => ReifyDd ('DdK sel mods a 'Prim) where
  reifyDd (Dd sel mods@(Mods ms) DdPrim) =
    DdTerm (pgColumnName name) (unSetTableName <$> maybeMod mods) (uncurry (Term.Prim (reifyPrimName @a ms)) (columnConstraints mods))
    where
      name = reifySel sel

instance (
    MaybeMod SetTableName mods,
    All ReifyDd sub
  ) => ReifyDd ('DdK sel mods a ('Comp ('SelSymbol tname) c i sub)) where
    reifyDd (Dd sel mods (DdComp (SelWSymbol Proxy) c i sub)) =
      DdTerm (pgColumnName name) (unSetTableName <$> maybeMod mods) (Term.Comp typeName (demoteComp c) (demoteInc i) (hcfoldMap (Proxy @ReifyDd) (pure . reifyDd) sub))
      where
        name = case sel of
          SelWSymbol (Proxy :: Proxy name) -> symbolText @name
          _ -> symbolText @tname
        typeName = symbolText @tname

-- instance (
--     FoldDd FoldPrim FoldComp DdTerm s
--   ) => ReifyDd s where
--     reifyDd dd = foldDd @FoldPrim @FoldComp foldPrim foldComp dd
