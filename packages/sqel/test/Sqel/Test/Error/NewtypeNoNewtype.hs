{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors -Wno-partial-type-signatures #-}

module Sqel.Test.Error.NewtypeNoNewtype where

import Generics.SOP (I (I), NP (Nil, (:*)))

import Sqel.Data.Dd
import Sqel.Data.Mods (Mods (Mods), Newtype (Newtype))
import Sqel.Prim (prim, primNewtype)
import Sqel.Product2 (prod)

data TextNt = TextNt { unTextNt :: Text }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    po :: TextNt
  }
  deriving stock (Eq, Show, Generic)

dd :: Dd (_ _ Dat _)
dd =
  prod (
    prim :>
    primNewtype
  )

useMod :: Text
useMod =
  case dd of
    Dd _ _ (DdComp _ _ _ (_ :* Dd _ (Mods (I (Newtype unwrap _) :* Nil)) _ :* Nil)) ->
      unwrap (TextNt "asdf")

newtypeNoNewtype :: Text
newtypeNoNewtype =
  useMod
