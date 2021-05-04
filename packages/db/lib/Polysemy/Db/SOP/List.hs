module Polysemy.Db.SOP.List where

import Fcf (Eval, Exp, Pure, type (@@))
import Fcf.Class.Functor (FMap)
import Generics.SOP (I(I), NP, K(K), htrans, hpure, All, Top, SameShapeAs)
import Generics.SOP.Constraint (AllZipF)

data ConstF :: a -> b -> Exp a
type instance Eval (ConstF a _) = Pure @@ a

type family As (a :: Type) (xs :: [k]) :: [l] where
  As a xs = FMap (ConstF a) @@ xs

type family AsUnit (xs :: [k]) :: [*] where
  AsUnit xs = As () xs

class KToI a x y where
  kToI :: K a x -> I y

instance KToI a x () where
  kToI (K _) =
    I ()

class NPAsUnit (xs :: [k]) (ys :: [*]) | xs -> ys where
  npAsUnit :: NP I ys

instance (
    All Top xs,
    All Top ys,
    SameShapeAs xs ys,
    SameShapeAs ys xs,
    AllZipF (KToI ()) xs ys,
    ys ~ AsUnit xs
  ) => NPAsUnit xs ys where
  npAsUnit =
    htrans (Proxy @(KToI ())) kToI (hpure (K ()) :: NP (K ()) xs)
