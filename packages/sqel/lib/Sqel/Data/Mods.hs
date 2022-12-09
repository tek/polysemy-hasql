module Sqel.Data.Mods where

import Exon (exon)
import Generics.SOP (All, Compose, I, NP (Nil), hcollapse, hcmap, K (K))
import Prelude hiding (Compose)
import Prettyprinter (Pretty (pretty), viaShow, hsep)
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

instance All Show ps => Pretty (Mods ps) where
  pretty (Mods ps) =
    hsep (hcollapse (hcmap (Proxy @Show) (K . viaShow) ps))

data EnumColumn = EnumColumn
  deriving stock (Eq, Show, Generic)

data ReadShowColumn = ReadShowColumn
  deriving stock (Eq, Show, Generic)

type ArrayColumn :: (Type -> Type) -> Type
data ArrayColumn f = ArrayColumn
  deriving stock (Eq, Show, Generic)
