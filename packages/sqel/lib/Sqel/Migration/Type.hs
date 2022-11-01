module Sqel.Migration.Type where

import Data.Some (Some (Some))
import Generics.SOP (NP ((:*)))

import Sqel.Data.Migration (MigrationAction (ModifyType))
import Sqel.Migration.Column (NewColumnsChanges (newColumnsChanges), OldColumnsChanges (oldColumnsChanges))
import Sqel.Migration.Data.Ddl (DdlType (DdlType), DdlTypeK (DdlTypeK))
import Sqel.SOP.HasGeneric (BoolVal (boolVal))

type TypeChange :: DdlTypeK -> DdlTypeK -> Constraint
class TypeChange old new where
  typeChange :: DdlType old -> DdlType new -> [MigrationAction]

instance TypeChange ('DdlTypeK table tname cols) ('DdlTypeK table tname cols) where
  typeChange _ _ = mempty

instance {-# overlappable #-} (
    BoolVal table,
    OldColumnsChanges colsOld colsNew,
    NewColumnsChanges colsOld colsNew
  ) => TypeChange ('DdlTypeK table tname colsOld) ('DdlTypeK table tname colsNew) where
  typeChange (DdlType name colsOld) (DdlType _ colsNew) =
    [ModifyType (boolVal @table) (Some name) (oldColumnsChanges colsOld colsNew <> newColumnsChanges colsOld colsNew)]

type OldTypeChanges :: DdlTypeK -> [DdlTypeK] -> Constraint
class OldTypeChanges old new where
  oldTypeChanges :: DdlType old -> NP DdlType new -> [MigrationAction]

instance (
    TypeChange ('DdlTypeK table tname colsOld) ('DdlTypeK table tname colsNew)
  ) => OldTypeChanges ('DdlTypeK table tname colsOld) ('DdlTypeK table tname colsNew : new) where
    oldTypeChanges old (new :* _) =
      typeChange old new

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
