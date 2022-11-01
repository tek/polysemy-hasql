module Sqel.Migration.Data.Ddl where

import Exon (exon)
import Generics.SOP (All, Compose, NP)
import Prelude hiding (Compose)
import Text.Show (showParen, showsPrec)

import Sqel.Data.Mods (Mods)
import Sqel.Data.PgType (ColumnType)
import Sqel.Data.PgTypeName (PgTypeName)
import Sqel.SOP.Constraint (symbolString)

data DdlColumnK =
  DdlColumnK {
    name :: Symbol,
    compName :: Maybe Symbol,
    params :: [Type],
    rename :: Maybe Symbol,
    delete :: Bool,
    tpe :: Type
  }

type DdlColumn :: DdlColumnK -> Type
data DdlColumn k where
  DdlColumn ::
    KnownSymbol name =>
    ColumnType ->
    Mods p ->
    DdlColumn ('DdlColumnK name comp p rename delete a)

instance (
    Show (Mods p)
  ) => Show (DdlColumn ('DdlColumnK name comp p rename delete a)) where
  showsPrec d (DdlColumn ctype p) =
    showParen (d > 10) [exon|DdlColumn #{showsPrec 11 (symbolString @name)} #{showsPrec 11 ctype} #{showsPrec 11 p}|]

data DdlTypeK =
  DdlTypeK {
    table :: Bool,
    tname :: Symbol,
    columns :: [DdlColumnK]
  }

type DdlType :: DdlTypeK -> Type
data DdlType s where
  DdlType :: KnownSymbol tname => PgTypeName table -> NP DdlColumn cols -> DdlType ('DdlTypeK table tname cols)

instance (
    All (Compose Show DdlColumn) cols
  ) => Show (DdlType ('DdlTypeK pgName tname cols)) where
  showsPrec d (DdlType name cols) =
    showParen (d > 10) [exon|DdlType #{showsPrec 11 name} #{showsPrec 11 cols}|]
