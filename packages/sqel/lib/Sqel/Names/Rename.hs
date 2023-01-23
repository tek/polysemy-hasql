module Sqel.Names.Rename where

import qualified Generics.SOP as SOP
import Generics.SOP (AllZipN, HTrans (htrans), NP)

import Sqel.Data.Dd (CompInc (Merge), Dd (Dd), DdK (DdK), DdStruct (DdComp), Struct (Comp, Prim))
import Sqel.Data.Sel (
  Sel (SelAuto, SelSymbol, SelUnused),
  SelW (SelWAuto, SelWSymbol, SelWUnused),
  TSel (TSel),
  TSelW (TSelW),
  TypeName,
  )

type RenameSel :: Sel -> Sel -> Constraint
class RenameSel s0 s1 where
  renameSel :: SelW s0 -> SelW s1

instance {-# overlappable #-} (
    s0 ~ s1
  ) => RenameSel s0 s1 where
  renameSel = id

instance (
    KnownSymbol name
  ) => RenameSel s0 ('SelSymbol name) where
  renameSel _ = SelWSymbol Proxy

instance RenameSel s0 'SelUnused where
  renameSel _ = SelWUnused

instance RenameSel s0 'SelAuto where
  renameSel _ = SelWAuto

type RenameTSel :: TSel -> TSel -> Constraint
class RenameTSel s0 s1 where
  renameTSel :: TSelW s0 -> TSelW s1

instance {-# overlappable #-} (
    s0 ~ s1
  ) => RenameTSel s0 s1 where
  renameTSel = id

instance (
    TypeName prefix tpe name
  ) => RenameTSel s0 ('TSel prefix tpe) where
  renameTSel _ = TSelW Proxy

type Rename :: DdK -> DdK -> Constraint
class Rename s0 s1 where
  rename :: Dd s0 -> Dd s1

instance (
    RenameSel sel0 sel1
  ) => Rename ('DdK sel0 p t 'Prim) ('DdK sel1 p t 'Prim) where
  rename (Dd sel p s) =
    Dd (renameSel sel) p s

instance {-# overlappable #-} (
    RenameSel sel0 sel1,
    RenameTSel tsel0 tsel1
  ) => Rename ('DdK sel0 p t ('Comp tsel0 c i sub)) ('DdK sel1 p t ('Comp tsel1 c i sub)) where
    rename (Dd sel p (DdComp tsel c i s)) =
      Dd (renameSel sel) p (DdComp (renameTSel tsel) c i s)

instance Rename ('DdK sel p t ('Comp tsel c 'Merge sub)) ('DdK sel p t ('Comp tsel c 'Merge sub)) where
    rename (Dd sel p (DdComp tsel c i s)) =
      Dd sel p (DdComp tsel c i s)

type RenameN :: ((DdK -> Type) -> k -> Type) -> k -> k -> Constraint
class RenameN h s0 s1 where
  renameN :: h Dd s0 -> h Dd s1

instance (
    HTrans h h,
    AllZipN (SOP.Prod h) Rename s0 s1
  ) => RenameN h s0 s1 where
  renameN = htrans (Proxy @Rename) rename

type Rename2 :: DdK -> DdK -> Constraint
class Rename2 s0 s1 where
  rename2 :: Dd s0 -> Dd s1

instance (
    RenameSel sel0 sel1
  ) => Rename2 ('DdK sel0 p t 'Prim) ('DdK sel1 p t 'Prim) where
  rename2 (Dd sel p s) = Dd (renameSel sel) p s

instance (
    RenameSel sel0 sel1,
    RenameTSel tsel0 tsel1,
    RenameN NP s0 s1
  ) => Rename2 ('DdK sel0 p t ('Comp tsel0 c i s0)) ('DdK sel1 p t ('Comp tsel1 c i s1)) where
  rename2 (Dd sel p (DdComp tsel c i sub)) = Dd (renameSel sel) p (DdComp (renameTSel tsel) c i (renameN sub))
