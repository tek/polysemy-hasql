module Sqel.Migration.Type where

import Data.Some (Some (Some))
import Generics.SOP (NP ((:*)))

import Sqel.Data.Migration (MigrationAction (ModifyType, RenameType), MigrationTypeAction)
import Sqel.Migration.Column (NewColumnsChanges (newColumnsChanges), OldColumnsChanges (oldColumnsChanges))
import Sqel.Migration.Data.Ddl (DdlColumn, DdlType (DdlType), DdlTypeK (DdlTypeK))
import Sqel.SOP.HasGeneric (BoolVal (boolVal))

typeColsChanges ::
  OldColumnsChanges old new =>
  NewColumnsChanges old new =>
  NP DdlColumn old ->
  NP DdlColumn new ->
  [MigrationTypeAction]
typeColsChanges colsOld colsNew =
  oldColumnsChanges colsOld colsNew <> newColumnsChanges colsOld colsNew

type TypeChange :: DdlTypeK -> DdlTypeK -> Constraint
class TypeChange old new where
  typeChange :: DdlType old -> DdlType new -> [MigrationAction]

-- TODO add an error message for when the name is mismatched, which might happen when the migration history type isn't
-- renamed
instance TypeChange ('DdlTypeK table tname renameOld cols) ('DdlTypeK table tname 'Nothing cols) where
  typeChange _ _ = mempty

instance {-# overlappable #-} (
    BoolVal table,
    OldColumnsChanges colsOld colsNew,
    NewColumnsChanges colsOld colsNew
  ) => TypeChange ('DdlTypeK table tname renameOld colsOld) ('DdlTypeK table tname renameNew colsNew) where
  typeChange (DdlType name colsOld) (DdlType _ colsNew) =
    [ModifyType (boolVal @table) (Some name) (typeColsChanges colsOld colsNew)]

type OldTypeChanges :: DdlTypeK -> [DdlTypeK] -> Constraint
class OldTypeChanges old new where
  oldTypeChanges :: DdlType old -> NP DdlType new -> [MigrationAction]

instance (
    TypeChange ('DdlTypeK table tname renameOld colsOld) ('DdlTypeK table tname renameNew colsNew)
  ) => OldTypeChanges ('DdlTypeK table tname renameOld colsOld) ('DdlTypeK table tname renameNew colsNew : new) where
    oldTypeChanges old (new :* _) =
      typeChange old new

instance (
    OldColumnsChanges colsOld colsNew,
    NewColumnsChanges colsOld colsNew
  ) => OldTypeChanges ('DdlTypeK 'False tnameOld renameOld colsOld) ('DdlTypeK 'False tnameNew ('Just tnameOld) colsNew : new) where
    oldTypeChanges (DdlType nameOld colsOld) (DdlType nameNew colsNew :* _) =
      RenameType nameOld nameNew : [ModifyType False (Some nameNew) (typeColsChanges colsOld colsNew)]

instance {-# overlappable #-} (
    OldTypeChanges old new
  ) => OldTypeChanges old (n : new) where
    oldTypeChanges old (_ :* new) =
      oldTypeChanges old new

-- TODO implement
type NewTypeChanges :: [DdlTypeK] -> DdlTypeK -> Constraint
class NewTypeChanges old new where

type TypeChanges :: [DdlTypeK] -> [DdlTypeK] -> Constraint
class TypeChanges old new where
  changes :: NP DdlType old -> NP DdlType new -> [MigrationAction]

instance TypeChanges '[] new where
  changes _ _ = mempty

instance (
    OldTypeChanges o new,
    TypeChanges old new
  ) => TypeChanges (o : old) new where
    changes (o :* old) new =
      oldTypeChanges o new <> changes old new
