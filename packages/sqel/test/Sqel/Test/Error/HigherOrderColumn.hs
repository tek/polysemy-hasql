{-# options_ghc -Wno-partial-type-signatures -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.HigherOrderColumn where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Data.Dd (Dd (Dd), DdK (DdK), DdStruct (DdComp), (:>) ((:>)), DdTypeSel, DdType)
import Sqel.Prim (prim, prims)
import Sqel.Product (prod, prodSel)
import Sqel.Type (Prim, Merge, type (>), TypeSel, Prod, type (*>))
import Sqel.Data.Sel (MkTSel)
import Sqel.Merge (merge)

data Pr =
  Pr {
    pr1 :: Text,
    pr2 :: Int,
    pr3 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Wrap a =
  Wrap { wrapped :: a, length :: Int64 }
  deriving stock (Eq, Show, Generic)

type WrapDd sa =
  TypeSel (DdTypeSel sa) (Prod (Wrap (DdType sa))) *> (
    Merge sa >
    Prim "length" Int64
  )

ddHO ::
  ∀ s merged .
  merged ~ Merge s =>
  MkTSel (DdTypeSel s) =>
  -- Column (DdType s) "wrapped" merged merged =>
  Dd s ->
  Dd (WrapDd s)
ddHO wrapped =
  prodSel @(DdTypeSel s) (merge wrapped :> prim)

ddHOCol :: Dd ('DdK _ _ (Wrap Pr) _)
ddHOCol =
  ddHO (prod prims)

higherOrderColumn :: ()
higherOrderColumn =
  case ddHOCol of
    Dd _ _ (DdComp _ _ _ ((Dd _ _ (DdComp _ _ _ (_ :* (Dd _ _ _) :* _ :* Nil))) :* _ :* Nil)) ->
      ()
