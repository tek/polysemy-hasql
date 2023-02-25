module Sqel.Migration.Type where

import Generics.SOP (NP (Nil, (:*)), SListI, Top, hcfoldMap)

import Sqel.Data.Migration (CompAction, TableAction, TypeAction (AddAction, ModifyAction, RenameAction))
import Sqel.Data.PgType (PgColumn (PgColumn), pgColumnName, PgColumns (PgColumns))
import Sqel.Data.PgTypeName (PgCompName)
import Sqel.Migration.Column (ColIndex (colIndex), ColumnsChanges (columnsChanges))
import Sqel.Migration.Data.Ddl (DdlColumn (DdlColumn), DdlColumnK, DdlType (DdlType), DdlTypeK (DdlTypeK))
import Sqel.SOP.Constraint (symbolText)

data OldK =
  OldK {
    table :: Bool,
    name :: Symbol,
    cols :: [DdlColumnK]
  }

data NewK =
  NewK {
    index :: Nat,
    table :: Bool,
    name :: Symbol,
    rename :: Maybe Symbol,
    cols :: [DdlColumnK]
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
  UnusedK

type family OldKs (index :: Nat) (types :: [DdlTypeK]) :: [OldK] where
  OldKs _ '[] = '[]
  OldKs index ('DdlTypeK table name _ cols : types) =
    'OldK table name cols : OldKs (index + 1) types

type family NewKs (index :: Nat) (types :: [DdlTypeK]) :: [NewK] where
  NewKs _ '[] = '[]
  NewKs index ('DdlTypeK table name rename cols : types) =
    'NewK index table name rename cols : NewKs (index + 1) types

type family MkMigrationAction (old :: OldK) (check :: [NewK]) (other :: [NewK]) :: (ActionK, [NewK]) where
  MkMigrationAction _ '[] other =
    '( 'UnusedK, other)
  MkMigrationAction ('OldK table name _) ('NewK index table name 'Nothing _ : news) other =
    '( 'ActionK 'KeepK index, news ++ other)
  MkMigrationAction ('OldK table oldName _) ('NewK index table _ ('Just oldName) _ : news) other =
    '( 'ActionK 'RenameK index, news ++ other)
  MkMigrationAction old (new : news) other =
    MkMigrationAction old news (new : other)

type family NewMigrationActions (cols :: [NewK]) :: [ActionK] where
  NewMigrationActions '[] = '[]
  NewMigrationActions ('NewK index 'False _ 'Nothing _ : news) =
    'ActionK 'AddK index : NewMigrationActions news
  NewMigrationActions cols = TypeError ("type NewMigrationActions:" % cols)

type family MigrationActionsCont (cur :: (ActionK, [NewK])) (old :: [OldK]) :: [ActionK] where
  MigrationActionsCont '(cur, new) old = cur : MigrationActions old new

type family MigrationActions (old :: [OldK]) (new :: [NewK]) :: [ActionK] where
  MigrationActions '[] rest =
    NewMigrationActions rest
  MigrationActions (old : olds) new =
    MigrationActionsCont (MkMigrationAction old new '[]) olds

type ReifyKeepAction :: Bool -> DdlTypeK -> DdlTypeK -> Constraint
class ReifyKeepAction table old new where
  reifyKeepAction :: DdlType old -> DdlType new -> TypeAction table

instance (
    ColumnsChanges colsOld colsNew
  ) => ReifyKeepAction table ('DdlTypeK table tname renameOld colsOld) ('DdlTypeK table tname renameNew colsNew) where
    reifyKeepAction (DdlType name colsOld) (DdlType _ colsNew) =
      ModifyAction name (columnsChanges colsOld colsNew)

type family ReifyModResult (table :: Bool) :: Type where
  ReifyModResult 'False =
    [(PgCompName, CompAction)]
  ReifyModResult 'True =
    TypeAction 'True

type ReifyModAction :: Bool -> ModK -> DdlTypeK -> DdlTypeK -> Constraint
class ReifyModAction table action old new where
  reifyModAction :: DdlType old -> DdlType new -> ReifyModResult table

instance (
    ReifyKeepAction 'True old new
  ) => ReifyModAction 'True 'KeepK old new where
    reifyModAction old new =
      reifyKeepAction @'True old new

instance (
    ReifyKeepAction 'False ('DdlTypeK 'False tname renameOld colsOld) new
  ) => ReifyModAction 'False 'KeepK ('DdlTypeK 'False tname renameOld colsOld) new where
    reifyModAction old@(DdlType name _) new =
      [(name, reifyKeepAction @'False old new)]

instance (
    ColumnsChanges colsOld colsNew
  ) => ReifyModAction 'False 'RenameK ('DdlTypeK 'False name renameOld colsOld) ('DdlTypeK 'False nameNew ('Just name) colsNew) where
    reifyModAction (DdlType nameOld colsOld) (DdlType nameNew colsNew) =
      [(nameOld, RenameAction nameNew (columnsChanges colsOld colsNew))]

type ReifyOldAction :: Bool -> ActionK -> DdlTypeK -> [DdlTypeK] -> Constraint
class ReifyOldAction table action old new where
  reifyOldAction :: DdlType old -> NP DdlType new -> ReifyModResult table

instance (
    ColIndex index news new,
    ReifyModAction table mod old new
  ) => ReifyOldAction table ('ActionK mod index) old news where
  reifyOldAction old news =
    reifyModAction @table @mod old (colIndex @index news)

instance ReifyOldAction 'False 'UnusedK ('DdlTypeK 'False name renameOld colsOld) new where
  reifyOldAction _ _ = []

-- -- TODO this has to check that new columns in composite types are
-- -- a) at the end of the list
-- -- b) Maybe
type ReifyNewAction :: ActionK -> [DdlTypeK] -> Constraint
class ReifyNewAction action new where
  reifyNewAction :: NP DdlType new -> (PgCompName, CompAction)

instance (
    SListI cols,
    ColIndex index news ('DdlTypeK 'False name rename cols)
  ) => ReifyNewAction ('ActionK 'AddK index) news where
  reifyNewAction news =
    let
      DdlType name ddlCols = colIndex @index news
      cols = hcfoldMap (Proxy @Top) \ (DdlColumn (Proxy :: Proxy n) t _) ->
        [PgColumn (pgColumnName (symbolText @n)) t]
    in (name, AddAction (PgColumns (cols ddlCols)))

type ReifyActions :: [ActionK] -> [DdlTypeK] -> [DdlTypeK] -> Constraint
class ReifyActions actions old new where
  reifyActions :: NP DdlType old -> NP DdlType new -> [(PgCompName, CompAction)]

instance ReifyActions '[] '[] new where
  reifyActions _ _ =
    mempty

instance (
    ReifyNewAction action new,
    ReifyActions actions '[] new
  ) => ReifyActions (action : actions) '[] new where
    reifyActions Nil new =
      reifyNewAction @action new : reifyActions @actions Nil new

instance (
    ReifyOldAction 'False action o new,
    ReifyActions actions old new
  ) => ReifyActions (action : actions) (o : old) new where
    reifyActions (o :* old) new =
      reifyOldAction @'False @action o new <> reifyActions @actions old new

type TypeChanges :: [DdlTypeK] -> [DdlTypeK] -> Constraint
class TypeChanges old new where
  typeChanges :: NP DdlType old -> NP DdlType new -> [(PgCompName, CompAction)]

instance (
    actions ~ MigrationActions (OldKs 0 old) (NewKs 0 new),
    ReifyActions actions old new
  ) => TypeChanges old new where
    typeChanges = reifyActions @actions

type TableChange :: DdlTypeK -> DdlTypeK -> Constraint
class TableChange old new where
  tableChange :: DdlType old -> DdlType new -> TableAction

instance (
    '[oldk] ~ OldKs 0 '[old],
    '(action, '[]) ~ MkMigrationAction oldk (NewKs 0 '[new]) '[],
    ReifyOldAction 'True action old '[new]
  ) => TableChange old new where
    tableChange old new =
      reifyOldAction @'True @action old (new :* Nil)
