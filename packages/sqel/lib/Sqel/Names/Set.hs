module Sqel.Names.Set where

import Fcf (Length, type (@@))
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage)
import Type.Errors.Pretty (type (%), type (<>))

import Sqel.Data.Dd (Comp (Sum), DdK (DdK), Struct (Comp, Prim))
import Sqel.Data.Sel (Sel (SelSymbol))
import Sqel.Names.Error (CountMismatch, NoPrimType)

type SetSel :: Sel -> Sel -> Sel
type family SetSel old new where
  SetSel _ new = new

type SetSelWithName :: Sel -> Symbol -> Sel
type family SetSelWithName s name where
  SetSelWithName old n = SetSel old ('SelSymbol n)

type SetName :: DdK -> Symbol -> DdK
type family SetName s name where
  SetName ('DdK sel p t s) n = 'DdK (SetSelWithName sel n) p t s

type SetNames :: ErrorMessage -> [DdK] -> [Symbol] -> [DdK]
type family SetNames error s0 names where
  SetNames _ '[] '[] = '[]
  SetNames error (s : ss) (n : names) = SetName s n : SetNames error ss names
  SetNames error _ _ = TypeError error

type SetNamesFor :: Symbol -> ErrorMessage -> [DdK] -> [Symbol] -> [DdK]
type family SetNamesFor desc a ss names where
  SetNamesFor desc a ss names =
    SetNames (CountMismatch desc a (Length @@ ss) (Length @@ names)) ss names

type SetTypeName :: DdK -> Symbol -> DdK
type family SetTypeName s name where
  SetTypeName ('DdK sel p t ('Comp tsel 'Sum i (index : sub))) n =
    'DdK sel p t ('Comp (SetSelWithName tsel n) 'Sum i (SetName index (AppendSymbol "ph_sum_index__" n) : sub))
  SetTypeName ('DdK sel p t ('Comp tsel c i sub)) n =
    'DdK sel p t ('Comp (SetSelWithName tsel n) c i sub)
  SetTypeName ('DdK _ _ t 'Prim) _ =
    NoPrimType t
  SetTypeName s _ =
    TypeError ("SetTypeName: " <> s)

type SetTypeSel :: DdK -> Sel -> DdK
type family SetTypeSel s sel where
  SetTypeSel ('DdK sel p t ('Comp tsel c i sub)) new =
    'DdK sel p t ('Comp (SetSel tsel new) c i sub)
  SetTypeSel ('DdK _ _ t 'Prim) _ =
    NoPrimType t
  SetTypeSel s sel =
    TypeError ("SetTypeSel:" % sel % s)
