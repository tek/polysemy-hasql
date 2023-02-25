module Sqel.Migration.Column where

import Generics.SOP (NP (Nil, (:*)), hd, tl)
import qualified Hasql.Encoders as Encoders

import Sqel.Class.Mods (OptMod (optMod))
import Sqel.Codec (PrimColumn (primEncoder))
import Sqel.Data.Migration (ColumnAction (AddColumn, RemoveColumn, RenameColumn))
import Sqel.Data.MigrationParams (MigrationDefault (MigrationDefault))
import Sqel.Data.PgType (ColumnType, PgColumnName, pgColumnName)
import Sqel.Migration.Data.Ddl (DdlColumn (DdlColumn), DdlColumnK (DdlColumnK))
import Sqel.SOP.Constraint (symbolText)

data OldK =
  OldK {
    name :: Symbol,
    comp :: Maybe Symbol,
    delete :: Bool
  }

data NewK =
  NewK {
    index :: Nat,
    name :: Symbol,
    comp :: Maybe Symbol,
    rename :: Maybe Symbol,
    renameType :: Maybe Symbol
  }

data ModK =
  KeepK
  |
  AddK
  |
  RenameK

data ActionK =
  ActionK {
    mod :: ModK,
    index :: Nat
  }
  |
  RemoveK

type family OldKs (index :: Nat) (cols :: [DdlColumnK]) :: [OldK] where
  OldKs _ '[] = '[]
  OldKs index ('DdlColumnK name comp _ _ _ delete _ : cols) =
    'OldK name comp delete : OldKs (index + 1) cols

type family NewKs (index :: Nat) (cols :: [DdlColumnK]) :: [NewK] where
  NewKs _ '[] = '[]
  NewKs index ('DdlColumnK name comp _ rename renameType _ _ : cols) =
    'NewK index name comp rename renameType : NewKs (index + 1) cols

-- TODO this reverses the other list every time
type family MkMigrationAction (old :: OldK) (check :: [NewK]) (other :: [NewK]) :: (ActionK, [NewK]) where
  MkMigrationAction ('OldK _ _ 'True) '[] other =
    '( 'RemoveK, other)
  MkMigrationAction ('OldK name comp 'False) ('NewK index name comp 'Nothing 'Nothing : news) other =
    '( 'ActionK 'KeepK index, news ++ other)
  MkMigrationAction ('OldK oldName comp 'False) ('NewK index _ comp ('Just oldName) 'Nothing : news) other =
    '( 'ActionK 'RenameK index, news ++ other)
  MkMigrationAction ('OldK oldName ('Just oldComp) 'False) ('NewK index newName _ rename ('Just oldComp) : news) other =
    MkMigrationAction ('OldK oldName ('Just oldComp) 'False) ('NewK index newName ('Just oldComp) rename 'Nothing : news) other
  MkMigrationAction old (new : news) other =
    MkMigrationAction old news (new : other)
  MkMigrationAction old '[] other =
    TypeError ("MkMigrationAction:" % old % other)

type family NewMigrationActions (cols :: [NewK]) :: [ActionK] where
  NewMigrationActions '[] = '[]
  NewMigrationActions ('NewK index _ _ 'Nothing 'Nothing : news) =
    'ActionK 'AddK index : NewMigrationActions news
  NewMigrationActions cols = TypeError ("NewMigrationActions:" % cols)

type family MigrationActionsCont (cur :: (ActionK, [NewK])) (old :: [OldK]) :: [ActionK] where
  MigrationActionsCont '(cur, new) old = cur : MigrationActions old new

-- TODO removing could be done implicitly, given that only renaming really _necessitates_ explicit marking.
-- The only reason to not do that is to avoid mistakes, but that seems exaggerated since we use unit tests for checking
-- consistency anyway, and integration tests to ensure the tables work
type family MigrationActions (old :: [OldK]) (new :: [NewK]) :: [ActionK] where
  MigrationActions '[] rest =
    NewMigrationActions rest
  MigrationActions (old : olds) new =
    MigrationActionsCont (MkMigrationAction old new '[]) olds

class ColumnAddition (comp :: Maybe Symbol) (def :: Type) where
  columnAddition :: def -> PgColumnName -> ColumnType -> [ColumnAction]

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

class ColIndex index cols col | index cols -> col where
  colIndex :: NP f cols -> f col

instance ColIndex 0 (col : cols) col where
  colIndex = hd

instance {-# overlappable #-} (
    ColIndex (n - 1) cols col
  ) => ColIndex n (c : cols) col where
  colIndex = colIndex @(n - 1) . tl

type ReifyModAction :: ModK -> DdlColumnK -> DdlColumnK -> Constraint
class ReifyModAction action old new where
  reifyModAction :: DdlColumn old -> DdlColumn new -> [ColumnAction]

instance ReifyModAction 'KeepK old new where
  reifyModAction _ _ = []

instance ReifyModAction 'RenameK ('DdlColumnK name compOld modsOld renameOld renameTOld deleteOld typeOld) ('DdlColumnK nameNew compNew modsNew ('Just name) renameTNew delNew typeNew) where
  reifyModAction (DdlColumn Proxy _ _) (DdlColumn Proxy _ _) =
    [RenameColumn (pgColumnName (symbolText @name)) (pgColumnName (symbolText @nameNew))]

type ReifyOldAction :: ActionK -> DdlColumnK -> [DdlColumnK] -> Constraint
class ReifyOldAction action old new where
  reifyOldAction :: DdlColumn old -> NP DdlColumn new -> [ColumnAction]

instance (
    ColIndex index news new,
    ReifyModAction mod old new
  ) => ReifyOldAction ('ActionK mod index) old news where
  reifyOldAction old news =
    reifyModAction @mod old new
    where
      new = colIndex @index news

instance ReifyOldAction 'RemoveK old new where
  reifyOldAction (DdlColumn (Proxy :: Proxy name) t _) _ =
    [RemoveColumn (pgColumnName (symbolText @name)) t]

-- -- TODO this has to check that new columns in composite types are
-- -- a) at the end of the list
-- -- b) Maybe
type ReifyNewAction :: ActionK -> [DdlColumnK] -> Constraint
class ReifyNewAction action new where
  reifyNewAction :: NP DdlColumn new -> [ColumnAction]

instance (
    ColIndex index news ('DdlColumnK name comp mods rename renameT delete tpe),
    OptMod (MigrationDefault tpe) mods def,
    ColumnAddition comp def
  ) => ReifyNewAction ('ActionK 'AddK index) news where
  reifyNewAction news =
    case colIndex @index news of
      DdlColumn (Proxy :: Proxy name) t mods ->
        columnAddition @comp @def (optMod @(MigrationDefault tpe) mods) (pgColumnName (symbolText @name)) t

type ReifyActions :: [ActionK] -> [DdlColumnK] -> [DdlColumnK] -> Constraint
class ReifyActions actions old new where
  reifyActions :: NP DdlColumn old -> NP DdlColumn new -> [ColumnAction]

instance ReifyActions '[] '[] new where
  reifyActions _ _ =
    mempty

instance (
    ReifyNewAction action new,
    ReifyActions actions '[] new
  ) => ReifyActions (action : actions) '[] new where
    reifyActions Nil new =
      reifyNewAction @action new <> reifyActions @actions Nil new

instance (
    ReifyOldAction action o new,
    ReifyActions actions old new
  ) => ReifyActions (action : actions) (o : old) new where
    reifyActions (o :* old) new =
      reifyOldAction @action o new <> reifyActions @actions old new

type ColumnsChanges :: [DdlColumnK] -> [DdlColumnK] -> Constraint
class ColumnsChanges old new where
  columnsChanges :: NP DdlColumn old -> NP DdlColumn new -> [ColumnAction]

instance (
    actions ~ MigrationActions (OldKs 0 old) (NewKs 0 new),
    ReifyActions actions old new
  ) => ColumnsChanges old new where
      columnsChanges = reifyActions @actions
