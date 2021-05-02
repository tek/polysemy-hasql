module Polysemy.Hasql.Column.Data.Effect where

import Fcf (Eval, Exp)
import Fcf.Class.Foldable (Any)
import Polysemy.Db.Data.Column (Flatten)
import Polysemy.Db.Tree.Meta (ADTMetadata)

data Tycon (f :: * -> *) (d :: *)
data Newtype (nt :: *) (d :: *)
data ADT (meta :: ADTMetadata) (rep :: *)
data CustomType (t :: Symbol)
data NoEffect

type family IsADTRep (f :: * -> *) (eff :: *) :: Bool where
  IsADTRep f (ADT _ (f _)) = 'True
  IsADTRep _ _ = 'False

data IsADTRepExp :: (* -> *) -> * -> Exp Bool
type instance Eval (IsADTRepExp f eff) =
  IsADTRep f eff

type family IsFlatten (eff :: *) :: Bool where
  IsFlatten eff =
    IsADTRep Flatten eff

data IsFlattenExp :: * -> Exp Bool
type instance Eval (IsFlattenExp eff) =
  IsFlatten eff

type family ContainsEffect (match :: * -> Exp Bool) (effs :: [*]) :: Bool where
  ContainsEffect match effs =
    Eval (Any match effs)

type family ContainsFlatten (effs :: [*]) :: Bool where
  ContainsFlatten effs =
    ContainsEffect (IsADTRepExp Flatten) effs
