module Sqel.Data.Mods where

import Exon (exon)
import Generics.SOP (All, Compose, I, K (K), NP (Nil), hcmap, hcollapse)
import Prelude hiding (Compose)
import Prettyprinter (Pretty (pretty), hsep, viaShow)
import Text.Show (show, showParen, showsPrec)

import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sql (Sql)

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

data Nullable = Nullable
  deriving stock (Show)

data Unique = Unique
  deriving stock (Show)

data PrimaryKey = PrimaryKey
  deriving stock (Show)

data PgDefault = PgDefault Sql
  deriving stock (Show)

data EnumColumn = EnumColumn
  deriving stock (Eq, Show, Generic)

data ReadShowColumn = ReadShowColumn
  deriving stock (Eq, Show, Generic)

type ArrayColumn :: (Type -> Type) -> Type
data ArrayColumn f = ArrayColumn
  deriving stock (Eq, Show, Generic)

newtype SetTableName =
  SetTableName { unSetTableName :: PgTableName }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

data Newtype a w =
  Newtype {
    unwrap :: a -> w,
    wrap :: w -> a
  }
  deriving stock (Generic)

instance Show (Newtype a w) where
  show _ =
    "Newtype"

data Ignore = Ignore
  deriving stock (Eq, Show, Generic)
