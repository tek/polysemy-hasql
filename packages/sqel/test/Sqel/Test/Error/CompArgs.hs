{-# options_ghc -Wno-partial-type-signatures -fdefer-type-errors -Wno-deferred-type-errors #-}

module Sqel.Test.Error.CompArgs where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.Column (nullable)
import Sqel.Data.Dd (Dd (Dd), DdK (DdK), DdStruct (DdComp), (:>) ((:>)))
import Sqel.Prim (prim)
import Sqel.Product (prod)

data Pr =
  Pr {
    pr1 :: Text,
    pr2 :: Int,
    pr3 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Maybe Text,
    p :: Pr
  }
  deriving stock (Eq, Show, Generic)

ddTooFew :: Dd ('DdK _ _ Dat _)
ddTooFew =
  prod (
    nullable prim :>
    prod prim
  )

prodTooFew :: ()
prodTooFew =
  case ddTooFew of
    Dd _ _ (DdComp _ _ _ (_ :* (Dd _ _ (DdComp _ _ _ (Dd _ _ _ :* _))) :* Nil)) ->
      ()
    Dd _ _ (DdComp _ _ _ (_ :* (Dd _ _ (DdComp _ _ _ _)) :* Nil)) ->
      ()

ddTooMany :: Dd ('DdK _ _ Dat _)
ddTooMany =
  prod (
    nullable prim :>
    prod (prim :> prim :> prim :> prim :> prim)
  )

prodTooMany :: ()
prodTooMany =
  case ddTooMany of
    Dd _ _ (DdComp _ _ _ (_ :* (Dd _ _ (DdComp _ _ _ (Dd _ _ _ :* _))) :* Nil)) ->
      ()

ddBadType :: Dd ('DdK _ _ Dat _)
ddBadType =
  prod (
    nullable prim :>
    prod (prim :> False :> prim)
  )

prodBadType :: ()
prodBadType =
  case ddBadType of
    Dd _ _ (DdComp _ _ _ (_ :* (Dd _ _ (DdComp _ _ _ (_ :* (Dd _ _ _) :* _ :* Nil))) :* Nil)) ->
      ()
