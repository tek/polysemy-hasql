module Polysemy.Hasql.Data.AlignColumns where

import GHC.TypeLits (AppendSymbol, Symbol)
import Generics.SOP.Type.Metadata (FieldInfo(FieldInfo))
import Type.Errors (ErrorMessage(Text, ShowType), TypeError)
import Type.Errors.Pretty (type (<>))

import Polysemy.Db.Data.Column (Flatten)
import Polysemy.Db.SOP.Constraint (IsRecord)

type family SameName_ d (n :: Symbol) (rn_ :: Symbol) (rn :: Symbol) :: Constraint where
  SameName_ d n n rn = ()
  SameName_ d n rn_ rn = TypeError ("data and rep field mismatch for " <> 'ShowType d <> ": " <> n <> " / " <> rn)

type family SameName d (n :: Symbol) (rn :: Symbol) :: Constraint where
  SameName d n n = ()
  SameName d n rn = SameName_ d n (AppendSymbol "_" rn) rn

type family JoinComma (ns :: [FieldInfo]) :: ErrorMessage where
  JoinComma ('FieldInfo n : n1 : ns) = 'Text n <> ", " <> JoinComma (n1 : ns)
  JoinComma '[ 'FieldInfo n] = 'Text n

type family Align d (ns :: [FieldInfo]) fs (rns :: [FieldInfo]) reps :: Constraint where
  Align d ('FieldInfo n : ns) (f : fs) ('FieldInfo rn : rns) (Flatten rep : reps) =
    (SameName d n rn, Align d ns fs rns reps, AlignColumns rep f)
  Align d ('FieldInfo n : ns) (f : fs) ('FieldInfo rn : rns) (rep : reps) =
    (SameName d n rn, Align d ns fs rns reps)
  Align d '[] '[] '[] '[] = ()
  Align d '[] '[] rns reps = TypeError ('Text "too many fields in rep for " <> 'ShowType d <> ": " <> JoinComma rns)
  Align d ns fs '[] reps = TypeError ('Text "missing fields in rep for " <> 'ShowType d <> ": " <> JoinComma ns)

class AlignColumns rep d where

instance (
    IsRecord d ds name ns,
    IsRecord rep reps rname rns,
    Align d ns ds rns reps
  ) => AlignColumns rep d where
