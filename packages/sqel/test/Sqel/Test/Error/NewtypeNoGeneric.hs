{-# options_ghc -fdefer-type-errors -Wno-deferred-type-errors -Wno-partial-type-signatures #-}

module Sqel.Test.Error.NewtypeNoGeneric where

import Generics.SOP (I (I), NP (Nil, (:*)))

import Sqel.Data.Dd
import Sqel.Data.Mods (Mods (Mods), Newtype (Newtype), NoMods)
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol))
import Sqel.Data.TableSchema (TableSchema)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primNewtype)
import Sqel.Product (prod)

newtype TextNt = TextNt { unTextNt :: Text }
  deriving stock (Eq, Show)

data Dat =
  Dat {
    name :: Text,
    po :: TextNt
  }
  deriving stock (Eq, Show, Generic)

dd :: Dd ('DdK 'SelAuto NoMods Dat ('Comp ('SelSymbol "Dat") ('Prod 'Reg) 'Nest [
  'DdK ('SelSymbol "name") NoMods Text 'Prim,
  'DdK ('SelSymbol "po") '[Newtype TextNt _] TextNt 'Prim
  ]))
dd =
  prod (
    prim :>
    primNewtype
  )

table :: TableSchema Dat
table =
  tableSchema dd

useMod :: Text
useMod =
  case dd of
    Dd _ _ (DdComp _ _ _ (_ :* Dd _ (Mods (I (Newtype unwrap _) :* Nil)) _ :* Nil)) ->
      unwrap (TextNt "asdf")

newtypeNoGeneric :: Text
newtypeNoGeneric =
  useMod
