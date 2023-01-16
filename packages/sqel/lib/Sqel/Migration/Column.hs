module Sqel.Migration.Column where

import Generics.SOP (NP (Nil, (:*)))
import qualified Hasql.Encoders as Encoders

import Sqel.Class.Mods (OptMod (optMod))
import Sqel.Codec (PrimColumn (primEncoder))
import Sqel.Data.Migration (MigrationTypeAction (AddColumn, RemoveColumn, RenameColumn))
import Sqel.Data.MigrationParams (MigrationDefault (MigrationDefault))
import Sqel.Data.Mods (Mods)
import Sqel.Data.PgType (ColumnType, PgColumnName, pgColumnName)
import Sqel.Migration.Data.Ddl (DdlColumn (DdlColumn), DdlColumnK (DdlColumnK))
import Sqel.SOP.Constraint (symbolText)

type ColumnChange :: DdlColumnK -> DdlColumnK -> Constraint
class ColumnChange old new where
  columnChange :: DdlColumn old -> DdlColumn new -> [MigrationTypeAction]

instance ColumnChange ('DdlColumnK name 'Nothing modsOld renameOld deleteOld typeOld) ('DdlColumnK name 'Nothing modsNew renameNew deleteNew typeOld) where
  columnChange _ _ = mempty

instance ColumnChange ('DdlColumnK name ('Just tname) modsOld renameOld deleteOld typeOld) ('DdlColumnK name ('Just tname) modsNew renameNew deleteNew typeNew) where
  columnChange _ _ = mempty

type ModsColumnChange :: [Type] -> DdlColumnK -> DdlColumnK -> Constraint
class ModsColumnChange mods old new where
  modsColumnChange :: Mods mods -> DdlColumn old -> DdlColumn new -> [MigrationTypeAction]

-- TODO error message when no column match was found for remove or rename
type OldColumnChanges :: DdlColumnK -> [DdlColumnK] -> Constraint
class OldColumnChanges old new where
  oldColumnChanges :: DdlColumn old -> NP DdlColumn new -> [MigrationTypeAction]

instance (
    ColumnChange ('DdlColumnK name compOld modsOld renameOld deleteOld typeOld) ('DdlColumnK name compNew modsNew renameNew deleteNew typeNew)
  ) => OldColumnChanges ('DdlColumnK name compOld modsOld renameOld deleteOld typeOld) ('DdlColumnK name compNew modsNew renameNew deleteNew typeNew : new) where
    oldColumnChanges old (new :* _) =
      columnChange old new

instance OldColumnChanges ('DdlColumnK name compOld modsOld renameOld deleteOld typeOld) ('DdlColumnK nameNew compNew modsNew ('Just name) delNew typeNew : new) where
    oldColumnChanges (DdlColumn _ _) (DdlColumn _ _ :* _) =
      [RenameColumn (pgColumnName (symbolText @name)) (pgColumnName (symbolText @nameNew))]

-- TODO error message when delete is False
instance OldColumnChanges ('DdlColumnK name comp modsOld rename 'True tpe) '[] where
  oldColumnChanges (DdlColumn t _) Nil =
    [RemoveColumn (pgColumnName (symbolText @name)) t]

instance {-# overlappable #-} (
    OldColumnChanges old new
  ) => OldColumnChanges old (n : new) where
    oldColumnChanges old (_ :* new) =
      oldColumnChanges old new

type OldColumnsChanges :: [DdlColumnK] -> [DdlColumnK] -> Constraint
class OldColumnsChanges old new where
  oldColumnsChanges :: NP DdlColumn old -> NP DdlColumn new -> [MigrationTypeAction]

instance OldColumnsChanges '[] new where
  oldColumnsChanges _ _ = mempty

instance (
    OldColumnChanges o new,
    OldColumnsChanges old new
  ) => OldColumnsChanges (o : old) new where
    oldColumnsChanges (o :* old) new =
      oldColumnChanges o new <> oldColumnsChanges old new

class ColumnAddition (comp :: Maybe Symbol) (def :: Type) where
  columnAddition :: def -> PgColumnName -> ColumnType -> [MigrationTypeAction]

instance ColumnAddition ('Just tname) () where
  columnAddition () n t = [AddColumn n t Nothing]

instance ColumnAddition 'Nothing () where
  columnAddition () n t = [AddColumn n t Nothing]

-- TODO error message when no migration default was specified for new column
-- TODO this encoder should be taken from the builder derivation
instance (
    PrimColumn a
  ) => ColumnAddition 'Nothing (MigrationDefault a) where
  columnAddition (MigrationDefault a) n t =
    [AddColumn n t md]
    where
      md = Just (a, Encoders.param (Encoders.nonNullable (primEncoder @a)))

type NewColumnChanges :: [DdlColumnK] -> DdlColumnK -> Constraint
class NewColumnChanges old new where
  newColumnChanges :: NP DdlColumn old -> DdlColumn new -> [MigrationTypeAction]

-- TODO matching the name here is duplicated in ColumnChange, should be ignored there?
instance (
    ColumnChange ('DdlColumnK tname comp modsOld renameOld deleteOld typeOld) ('DdlColumnK tname comp modsNew 'Nothing deleteNew typeNew)
  ) => NewColumnChanges ('DdlColumnK tname comp modsOld renameOld deleteOld typeOld : old) ('DdlColumnK tname comp modsNew 'Nothing deleteNew typeNew) where
    newColumnChanges (old :* _) new =
      columnChange old new

instance NewColumnChanges ('DdlColumnK tnameOld comp modsOld renameOld deleteOld typeOld : old) ('DdlColumnK tnameNew comp modsNew ('Just tnameOld) deleteNew typeNew) where
    newColumnChanges _ _ =
      mempty

instance (
    OptMod (MigrationDefault tpe) mods def,
    ColumnAddition comp def
  ) => NewColumnChanges '[] ('DdlColumnK name comp mods rename delete tpe) where
    newColumnChanges Nil (DdlColumn t mods) =
      columnAddition @comp @def (optMod @(MigrationDefault tpe) mods) (pgColumnName (symbolText @name)) t

instance {-# overlappable #-} (
    NewColumnChanges old new
  ) => NewColumnChanges (o : old) new where
    newColumnChanges (_ :* old) new =
      newColumnChanges old new

-- TODO this has to check that new columns in composite types are
-- a) at the end of the list
-- b) Maybe
type NewColumnsChanges :: [DdlColumnK] -> [DdlColumnK] -> Constraint
class NewColumnsChanges old new where
  newColumnsChanges :: NP DdlColumn old -> NP DdlColumn new -> [MigrationTypeAction]

instance NewColumnsChanges old '[] where
  newColumnsChanges _ _ = mempty

instance (
    NewColumnChanges old n,
    NewColumnsChanges old new
  ) => NewColumnsChanges old (n : new) where
    newColumnsChanges old (n :* new) =
      newColumnChanges old n <> newColumnsChanges old new
