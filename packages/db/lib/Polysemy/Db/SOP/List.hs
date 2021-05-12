module Polysemy.Db.SOP.List where

import Fcf (ConstFn, Eval, Exp, If, IsJust, Pure, type (@@))
import Fcf.Class.Functor (FMap)
import Generics.SOP (All, K(K), NP, SameShapeAs, Top, hpure, htrans)
import Generics.SOP.Constraint (AllZipF)

type family As (a :: Type) (xs :: [k]) :: [l] where
  As a xs = FMap (ConstFn a) @@ xs

type family AsUnit (xs :: [k]) :: [Type] where
  AsUnit xs = As () xs

class KToI f a x y where
  kToI :: K a x -> f y

instance Applicative f => KToI f a x () where
  kToI (K _) =
    pure ()

class NPAsUnit (f :: Type -> Type) (xs :: [k]) (ys :: [Type]) | xs -> ys where
  npAsUnit :: NP f ys

instance (
    All Top xs,
    All Top ys,
    SameShapeAs xs ys,
    SameShapeAs ys xs,
    AllZipF (KToI f ()) xs ys,
    ys ~ AsUnit xs
  ) => NPAsUnit f xs ys where
  npAsUnit =
    htrans (Proxy @(KToI f ())) kToI (hpure (K ()) :: NP (K ()) xs)

data FirstJust :: (a -> Exp (Maybe b)) -> [a] -> Exp (Maybe b)
type instance Eval (FirstJust _ '[]) =
  'Nothing
type instance Eval (FirstJust p (a : as)) =
  If (IsJust @@ (p @@ a)) (Pure (p @@ a)) @@ (FirstJust p as)
