module Sqel.Names.Amend where

import Fcf (Length, type (@@))
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage)

import Sqel.Data.Dd (CompInc (Merge), DdK (DdK), Struct (Comp, Prim))
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol))
import Sqel.Names.Error (CountMismatch, NoPrimType)

type AmendSel :: Sel -> Sel -> Sel
type family AmendSel old new where
  AmendSel 'SelAuto new = new
  AmendSel old _ = old

type AmendSelWithName :: Sel -> Symbol -> Sel
type family AmendSelWithName s name where
  AmendSelWithName old n = AmendSel old ('SelSymbol n)

type AmendName :: DdK -> Symbol -> DdK
type family AmendName s name where
  AmendName ('DdK sel p t ('Comp tsel c 'Merge s)) _ = 'DdK sel p t ('Comp tsel c 'Merge s)
  AmendName ('DdK sel p t s) n = 'DdK (AmendSelWithName sel n) p t s

type AmendNames :: ErrorMessage -> [DdK] -> [Symbol] -> [DdK]
type family AmendNames error s0 names where
  AmendNames _ '[] '[] = '[]
  AmendNames error (s : ss) (n : names) = AmendName s n : AmendNames error ss names
  AmendNames error _ _ = TypeError error

type AmendNamesFor :: Symbol -> ErrorMessage -> [DdK] -> [Symbol] -> [DdK]
type family AmendNamesFor desc a ss names where
  AmendNamesFor desc a ss names =
    AmendNames (CountMismatch desc a (Length @@ ss) (Length @@ names)) ss names

type AmendTypeName :: DdK -> Symbol -> DdK
type family AmendTypeName s name where
  AmendTypeName ('DdK sel p t ('Comp tsel c i sub)) n =
    'DdK sel p t ('Comp (AmendSelWithName tsel n) c i sub)
  AmendTypeName ('DdK _ _ t 'Prim) _ =
    NoPrimType t
  AmendTypeName s _ =
    TypeError ("AmendTypeName: " <> s)

type AmendTypeSel :: DdK -> Sel -> DdK
type family AmendTypeSel s sel where
  AmendTypeSel ('DdK sel p t ('Comp tsel c i sub)) new =
    'DdK sel p t ('Comp (AmendSel tsel new) c i sub)
  AmendTypeSel ('DdK _ _ t 'Prim) _ =
    NoPrimType t
  AmendTypeSel s sel =
    TypeError ("AmendTypeSel:" % sel % s)
