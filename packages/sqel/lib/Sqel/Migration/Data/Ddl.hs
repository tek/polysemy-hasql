module Sqel.Migration.Data.Ddl where

import Exon (exon)
import Generics.SOP (All, Compose, NP)
import Prelude hiding (Compose)

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
    renameType :: Maybe Symbol,
    delete :: Bool,
    tpe :: Type
  }

type DdlColumn :: DdlColumnK -> Type
data DdlColumn k where
  DdlColumn ::
    KnownSymbol name =>
    ColumnType ->
    Mods p ->
    DdlColumn ('DdlColumnK name comp p rename renameType delete a)

instance (
    Show (Mods p)
  ) => Show (DdlColumn ('DdlColumnK name comp p rename renameType delete a)) where
  showsPrec d (DdlColumn ctype p) =
    showParen (d > 10) [exon|DdlColumn #{showsPrec 11 (symbolString @name)} #{showsPrec 11 ctype} #{showsPrec 11 p}|]

data DdlTypeK =
  DdlTypeK {
    table :: Bool,
    tname :: Symbol,
    rename :: Maybe Symbol,
    columns :: [DdlColumnK]
  }

type DdlType :: DdlTypeK -> Type
data DdlType s where
  DdlType :: KnownSymbol tname => PgTypeName table -> NP DdlColumn cols -> DdlType ('DdlTypeK table tname rename cols)

instance (
    All (Compose Show DdlColumn) cols
  ) => Show (DdlType ('DdlTypeK pgName tname rename cols)) where
  showsPrec d (DdlType name cols) =
    showParen (d > 10) [exon|DdlType #{showsPrec 11 name} #{showsPrec 11 cols}|]
