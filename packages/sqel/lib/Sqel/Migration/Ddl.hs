module Sqel.Migration.Ddl where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.ColumnConstraints (ColumnConstraints, columnConstraints)
import Sqel.Data.Dd (CompInc (Merge, Nest), Dd (Dd), DdK (DdK), DdStruct (DdComp, DdPrim), Struct (Comp, Prim))
import Sqel.Data.MigrationParams (MigrationDeleteK, MigrationRenameK, MigrationRenameTypeK)
import Sqel.Data.Mods (Mods (Mods))
import Sqel.Data.PgType (ColumnType (ColumnComp, ColumnPrim), pgTypeRefSym)
import Sqel.Data.PgTypeName (MkPgTypeName (pgTypeName))
import Sqel.Data.Sel (ReifySel, Sel (SelSymbol), SelW (SelWSymbol), TSel (TSel), TSelW (TSelW), TypeName)
import Sqel.Migration.Data.Ddl (DdlColumn (DdlColumn), DdlColumnK (DdlColumnK), DdlType (DdlType), DdlTypeK (DdlTypeK))
import Sqel.ReifyDd (ReifyPrimName (reifyPrimName))

appendNP :: NP f as -> NP f bs -> NP f (as ++ bs)
appendNP Nil bs =
  bs
appendNP (h :* t) bs =
  h :* appendNP t bs

type DdCols :: [DdK] -> [DdlColumnK] -> [DdlTypeK] -> Constraint
class DdCols s cols types | s -> cols types where
  ddCols :: NP Dd s -> (NP DdlColumn cols, NP DdlType types)

instance DdCols '[] '[] '[] where
  ddCols Nil = (Nil, Nil)

-- TODO the migration params could be extracted in OldColumnsChanges and passed to OldColumnChanges.
instance (
    ReifySel sel name,
    ReifyPrimName a mods,
    ColumnConstraints mods,
    DdCols ss cols types,
    rename ~ MigrationRenameK mods,
    renameType ~ MigrationRenameTypeK mods,
    delete ~ MigrationDeleteK mods
  ) => DdCols ('DdK sel mods a 'Prim : ss) ('DdlColumnK name 'Nothing mods rename renameType delete a : cols) types where
    ddCols (Dd _ m@(Mods mods) DdPrim :* t) =
      (DdlColumn Proxy (ColumnPrim (reifyPrimName @a mods) unique constr) m :* cols, types)
      where
        (unique, constr) = columnConstraints m
        (cols, types) = ddCols t

instance (
    ColumnConstraints mods,
    DdlTypes 'False ('DdK ('SelSymbol name) mods a ('Comp ('TSel tprefix tname) c 'Nest sub)) hTypes,
    DdCols ss cols types,
    allTypes ~ hTypes ++ types,
    rename ~ MigrationRenameK mods,
    renameType ~ MigrationRenameTypeK mods,
    delete ~ MigrationDeleteK mods,
    TypeName tprefix tname pgName
  ) => DdCols ('DdK ('SelSymbol name) mods a ('Comp ('TSel tprefix tname) c 'Nest sub) : ss) ('DdlColumnK name ('Just pgName) mods rename renameType delete a : cols) allTypes where
    ddCols (h@(Dd (SelWSymbol Proxy) mods (DdComp (TSelW Proxy) _ _ _)) :* t) =
      (DdlColumn Proxy (ColumnComp (pgTypeRefSym @pgName) unique constr) mods :* tailCols, appendNP subTypes tailTypes)
      where
        (unique, constr) = columnConstraints mods
        subTypes = ddTypes @'False @_ @hTypes h
        (tailCols, tailTypes) = ddCols t

instance (
    DdCols sub mergeCols subTypes,
    DdCols ss cols types,
    allCols ~ mergeCols ++ cols,
    allTypes ~ subTypes ++ types
  ) => DdCols ('DdK sel mods a ('Comp ('TSel tprefix tname) c 'Merge sub) : ss) allCols allTypes where
    ddCols (Dd _ _ (DdComp _ _ _ sub) :* t) =
      (appendNP subCols tailCols, appendNP subTypes tailTypes)
      where
        (subCols, subTypes) = ddCols sub
        (tailCols, tailTypes) = ddCols t

type DdlTypes :: Bool -> DdK -> [DdlTypeK] -> Constraint
class DdlTypes table s types | table s -> types where
  ddTypes :: Dd s -> NP DdlType types

instance (
    DdCols sub cols types,
    rename ~ MigrationRenameTypeK mods,
    MkPgTypeName tprefix tname table pgName
  ) => DdlTypes table ('DdK sel mods a ('Comp ('TSel tprefix tname) c i sub)) ('DdlTypeK table pgName rename cols : types) where
  ddTypes (Dd _ _ (DdComp (TSelW Proxy) _ _ sub)) =
    DdlType (pgTypeName @tprefix @tname) cols :* types
    where
      (cols, types) = ddCols sub

ddTable ::
  DdlTypes 'True s (table : types) =>
  Dd s ->
  (DdlType table, NP DdlType types)
ddTable dd =
  (table, types)
  where
    table :* types = ddTypes @'True dd
