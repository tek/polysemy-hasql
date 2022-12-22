module Sqel.Names.Set where

import Fcf (Length, type (@@))
import Prelude hiding (type (@@))
import Type.Errors (ErrorMessage)
import Type.Errors.Pretty (type (%), type (<>))

import Sqel.Data.Dd (DdK (DdK), Struct (Comp, Prim))
import Sqel.Data.Sel (Sel (SelSymbol))
import Sqel.Names.Error (CountMismatch, NoPrimType)

type SetSel :: DdK -> Sel -> DdK
type family SetSel s sel where
  SetSel ('DdK _ p t s) sel = 'DdK sel p t s

type SetName :: DdK -> Symbol -> DdK
type family SetName s name where
  SetName s n = SetSel s ('SelSymbol n)

type SetNames :: ErrorMessage -> [DdK] -> [Symbol] -> [DdK]
type family SetNames error s0 names where
  SetNames _ '[] '[] = '[]
  SetNames error (s : ss) (n : names) = SetName s n : SetNames error ss names
  SetNames error _ _ = TypeError error

type SetNamesFor :: Symbol -> ErrorMessage -> [DdK] -> [Symbol] -> [DdK]
type family SetNamesFor desc a ss names where
  SetNamesFor desc a ss names =
    SetNames (CountMismatch desc a (Length @@ ss) (Length @@ names)) ss names

-- TODO is that case for the index relevant anymore?
type SetTypeName :: DdK -> Symbol -> DdK
type family SetTypeName s name where
  -- SetTypeName ('DdK sel p t ('Comp _ 'Sum i (index : sub))) n =
  --   'DdK sel p t ('Comp ('SelSymbol n) 'Sum i (SetName index (AppendSymbol "ph_sum_index__" n) : sub))
  SetTypeName ('DdK sel p t ('Comp _ c i sub)) n =
    'DdK sel p t ('Comp ('SelSymbol n) c i sub)
  SetTypeName ('DdK _ _ t 'Prim) _ =
    NoPrimType t
  SetTypeName s _ =
    TypeError ("SetTypeName: " <> s)

type SetTypeSel :: DdK -> Sel -> DdK
type family SetTypeSel s sel where
  SetTypeSel ('DdK sel p t ('Comp _ c i sub)) new =
    'DdK sel p t ('Comp new c i sub)
  SetTypeSel ('DdK _ _ t 'Prim) _ =
    NoPrimType t
  SetTypeSel s sel =
    TypeError ("SetTypeSel:" % sel % s)