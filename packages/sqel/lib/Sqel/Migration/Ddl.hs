module Sqel.Migration.Ddl where

import Generics.SOP (NP (Nil, (:*)))

import Sqel.ColumnConstraints (ColumnConstraints, columnConstraints)
import Sqel.Data.Dd (CompInc (Merge, Nest), Dd (Dd), DdK (DdK), DdStruct (DdComp, DdPrim), Struct (Comp, Prim))
import Sqel.Data.MigrationParams (MigrationDeleteK, MigrationRenameK)
import Sqel.Data.Mods (Mods (Mods))
import Sqel.Data.PgType (ColumnType (ColumnComp, ColumnPrim), pgTypeRefSym)
import Sqel.Data.PgTypeName (MkPgTypeName (pgTypeName))
import Sqel.Data.Sel (Sel (SelSymbol), SelW (SelWSymbol))
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
    ReifyPrimName a mods,
    ColumnConstraints mods,
    DdCols ss cols types,
    rename ~ MigrationRenameK mods,
    delete ~ MigrationDeleteK mods
  ) => DdCols ('DdK ('SelSymbol name) mods a 'Prim : ss) ('DdlColumnK name 'Nothing mods rename delete a : cols) types where
    ddCols (Dd (SelWSymbol Proxy) m@(Mods mods) DdPrim :* t) =
      (DdlColumn (ColumnPrim (reifyPrimName @a mods) unique constr) m :* cols, types)
      where
        (unique, constr) = columnConstraints m
        (cols, types) = ddCols t

instance (
    DdlTypes 'False ('DdK ('SelSymbol name) p a ('Comp ('SelSymbol tname) c 'Nest sub)) hTypes,
    DdCols ss cols types,
    allTypes ~ hTypes ++ types,
    rename ~ MigrationRenameK p,
    delete ~ MigrationDeleteK p
  ) => DdCols ('DdK ('SelSymbol name) p a ('Comp ('SelSymbol tname) c 'Nest sub) : ss) ('DdlColumnK name ('Just tname) p rename delete a : cols) allTypes where
    ddCols (h@(Dd (SelWSymbol Proxy) p (DdComp (SelWSymbol Proxy) _ _ _)) :* t) =
      (DdlColumn (ColumnComp (pgTypeRefSym @tname)) p :* tailCols, appendNP subTypes tailTypes)
      where
        subTypes = ddTypes @'False @_ @hTypes h
        (tailCols, tailTypes) = ddCols t

instance (
    DdCols sub mergeCols subTypes,
    DdCols ss cols types,
    allCols ~ mergeCols ++ cols,
    allTypes ~ subTypes ++ types
  ) => DdCols ('DdK sel p a ('Comp ('SelSymbol tname) c 'Merge sub) : ss) allCols allTypes where
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
    MkPgTypeName tname table
  ) => DdlTypes table ('DdK sel p a ('Comp ('SelSymbol tname) c i sub)) ('DdlTypeK table tname cols : types) where
  ddTypes (Dd _ _ (DdComp (SelWSymbol Proxy) _ _ sub)) =
    DdlType (pgTypeName @tname) cols :* types
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
