module Sqel.ReifyDd where

import Generics.SOP (I (I), NP (Nil, (:*)), tl)

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
import Sqel.Data.Sel (ReifySel (reifySel), SelW (SelWSymbol), TSelW (TSelW))
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
    ReifySel sel name
  ) => ReifyDd ('DdK sel mods a 'Prim) where
    reifyDd (Dd sel mods@(Mods ms) DdPrim) =
      DdTerm (pgColumnName name) (unSetTableName <$> maybeMod mods) unique constraints (Term.Prim (reifyPrimName @a ms))
      where
        (unique, constraints) = columnConstraints mods
        name = reifySel sel

instance (
    ColumnConstraints mods,
    MaybeMod SetTableName mods,
    ReifyDdComp sub
  ) => ReifyDd ('DdK sel mods a ('Comp tsel c i sub)) where
    reifyDd (Dd sel mods (DdComp (TSelW (Proxy :: Proxy '(tname, tpe))) c i sub)) =
      DdTerm (pgColumnName name) (unSetTableName <$> maybeMod mods) unique constraints struct
      where
        (unique, constraints) = columnConstraints mods
        struct = Term.Comp typeName (demoteComp c) (demoteInc i) (reifyDdComp sub)
        name = case sel of
          SelWSymbol (Proxy :: Proxy name) -> symbolText @name
          _ -> symbolText @tname
        typeName = symbolText @tpe

-- TODO this is probably only necessary because of a bug in GHC that's fixed in master
class ReifyDdComp s where
  reifyDdComp :: NP Dd s -> [DdTerm]

instance ReifyDdComp '[] where
  reifyDdComp Nil = []

instance (
    ReifyDd s,
    ReifyDdComp ss
  ) => ReifyDdComp (s : ss) where
    reifyDdComp (s :* ss) = reifyDd s : reifyDdComp ss
