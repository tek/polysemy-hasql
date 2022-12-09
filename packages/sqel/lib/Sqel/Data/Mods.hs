module Sqel.Data.Mods where

import Exon (exon)
import Generics.SOP (All, Compose, I, NP (Nil))
import Prelude hiding (Compose)
import Text.Show (showParen, showsPrec)

newtype Mods ps = Mods { unMods :: NP I ps }

type NoMods = '[]

pattern NoMods :: () => (ps ~ '[]) => Mods ps
pattern NoMods = Mods Nil

instance (
    All (Compose Show I) ps
  ) => Show (Mods ps) where
  showsPrec d (Mods ps) =
    showParen (d > 10) [exon|Mods #{showsPrec 11 ps}|]

data EnumColumn = EnumColumn
  deriving stock (Eq, Show, Generic)

data ReadShowColumn = ReadShowColumn
  deriving stock (Eq, Show, Generic)

type ArrayColumn :: (Type -> Type) -> Type
data ArrayColumn f = ArrayColumn
  deriving stock (Eq, Show, Generic)
