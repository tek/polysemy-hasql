module Polysemy.Db.Tree.Data.Effect where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Foldable (Any)

import Polysemy.Db.Data.Rep (Flatten, PrimQuery, PrimQueryAs)
import Polysemy.Db.SOP.List (FirstJust)
import Polysemy.Db.Tree.Meta (AdtMetadata)

data Tycon (f :: Type -> Type) (d :: Type)
data Newtype (nt :: Type) (d :: Type)
data Adt (meta :: AdtMetadata) (rep :: Type)
data CustomType (t :: Symbol)
data NoEffect

type family ContainsEffect (match :: Type -> Exp Bool) (effs :: [Type]) :: Bool where
  ContainsEffect match effs =
    Eval (Any match effs)

type family IsADTRep (f :: Type -> Type) (eff :: Type) :: Bool where
  IsADTRep f (Adt _ (f _)) = 'True
  IsADTRep _ _ = 'False

data IsADTRepExp :: (Type -> Type) -> Type -> Exp Bool
type instance Eval (IsADTRepExp f eff) =
  IsADTRep f eff

type family ContainsFlatten (effs :: [Type]) :: Bool where
  ContainsFlatten effs =
    ContainsEffect (IsADTRepExp Flatten) effs

type family FindEffect (match :: Type -> Exp (Maybe a)) (effs :: [Type]) :: Maybe a where
  FindEffect match effs =
    FirstJust match @@ effs

type GetAdtRep :: (Type -> a) -> Type -> Maybe a
type family GetAdtRep (f :: Type -> a) (eff :: Type) :: Maybe a where
  GetAdtRep f (Adt _ (f a)) = 'Just a
  GetAdtRep _ _ = 'Nothing

data GetADTRepF :: (Type -> a) -> Type -> Exp (Maybe a)
type instance Eval (GetADTRepF f eff) =
  GetAdtRep f eff

type family FindFlatten (effs :: [Type]) :: Maybe Type where
  FindFlatten effs =
    FindEffect (GetADTRepF Flatten) effs

type family GetPrimQuery (eff :: Type) :: Maybe Symbol where
  GetPrimQuery (PrimQuery name) =
    'Just name
  GetPrimQuery (PrimQueryAs name _) =
    'Just name
  GetPrimQuery _ =
    'Nothing

data GetPrimQueryF :: Type -> Exp (Maybe Symbol)
type instance Eval (GetPrimQueryF eff) =
  GetPrimQuery eff

type family FindPrimQuery (effs :: [Type]) :: Maybe Symbol where
  FindPrimQuery effs =
    FindEffect GetPrimQueryF effs
