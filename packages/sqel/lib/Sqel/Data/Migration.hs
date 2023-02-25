module Sqel.Data.Migration where

import Exon (exon)
import Generics.SOP (NP (Nil, (:*)))
import Hasql.Encoders (Params)

import Sqel.Data.Dd ((:>) ((:>)))
import Sqel.Data.PgType (ColumnType, PgColumnName, PgColumns, PgTable)
import Sqel.Data.PgTypeName (PgCompName, PgTableName, PgTypeName)

data ColumnAction where
  AddColumn :: PgColumnName -> ColumnType -> Maybe (a, Params a) -> ColumnAction
  RemoveColumn :: PgColumnName -> ColumnType -> ColumnAction
  RenameColumn :: PgColumnName -> PgColumnName -> ColumnAction
  RenameColumnType :: PgColumnName -> PgCompName -> ColumnAction

instance Show ColumnAction where
  showsPrec d =
    showParen (d > 10) . \case
      AddColumn name tpe _ ->
        [exon|AddColumn #{showsPrec 11 name} #{showsPrec 11 tpe}|]
      RemoveColumn name tpe ->
        [exon|RemoveColumn #{showsPrec 11 name} #{showsPrec 11 tpe}|]
      RenameColumn old new ->
        [exon|RenameColumn #{showsPrec 11 old} #{showsPrec 11 new}|]
      RenameColumnType name new ->
        [exon|RenameColumnType #{showsPrec 11 name} #{showsPrec 11 new}|]

data TypeAction (table :: Bool) where
  ModifyAction :: PgTypeName table -> [ColumnAction] -> TypeAction table
  RenameAction :: PgCompName -> [ColumnAction] -> TypeAction 'False
  AddAction :: PgColumns -> TypeAction 'False

type TableAction = TypeAction 'True
type CompAction = TypeAction 'False

data MigrationActions ext =
  AutoActions {
    table :: TableAction,
    types :: Map PgCompName CompAction
  }
  |
  CustomActions ext
  deriving stock (Generic)

data Mig =
  Mig {
    from :: Type,
    to :: Type,
    effect :: Type -> Type,
    ext :: Type
  }

type Migration :: Mig -> Type
data Migration t where
  Migration :: {
    tableFrom :: PgTable from,
    tableTo :: PgTable to,
    actions :: MigrationActions ext
  } -> Migration ('Mig from to m ext)

type family MigFrom (mig :: Mig) :: Type where
  MigFrom ('Mig from _ _ _) = from

type family MigTo (mig :: Mig) :: Type where
  MigTo ('Mig _ to _ _) = to

type family MigEff (mig :: Mig) :: Type -> Type where
  MigEff ('Mig _ _ m _) = m

type family MigExt (mig :: Mig) :: Type where
  MigExt ('Mig _ _ _ ext) = ext

type UniMigList :: (Type -> Type) -> Type -> [Type] -> [Mig]
type family UniMigList m ext as where
  UniMigList _ _ '[] = '[]
  UniMigList m ext [new, old] = '[ 'Mig old new m ext]
  UniMigList m ext (new : old : as) = 'Mig old new m ext : UniMigList m ext (old : as)

type UniMigs :: (Type -> Type) -> Type -> [Type] -> Type -> [Mig]
type family UniMigs m ext old cur where
  UniMigs _ _ '[] _ = '[]
  UniMigs m ext '[o] cur = '[ 'Mig o cur m ext]
  UniMigs m ext (o : os) cur = 'Mig o cur m ext : UniMigList m ext (o : os)

type Migrations :: (Type -> Type) -> [Mig] -> Type
newtype Migrations m migs =
  Migrations { unMigrations :: NP Migration migs }

type UniMigrations m ext old cur =
  Migrations m (UniMigs m ext old cur)

type AutoMigrations m old cur =
  UniMigrations m Void old cur

class MkMigrations arg migs | arg -> migs, migs -> arg where
  mkMigrations :: arg -> NP Migration migs

instance (
    MkMigrations old (mig1 : migs)
  ) => MkMigrations (Migration ('Mig from to m ext) :> old) ('Mig from to m ext : mig1 : migs) where
    mkMigrations (next :> old) =
      next :* mkMigrations old

instance MkMigrations (Migration ('Mig from to m ext)) '[ 'Mig from to m ext] where
  mkMigrations next =
    next :* Nil

migrate ::
  MkMigrations arg migs =>
  arg ->
  Migrations m migs
migrate =
  Migrations . mkMigrations

noMigrations :: Migrations m '[]
noMigrations =
  Migrations Nil

class CustomMigration m mig where
  customTypeKeys :: MigExt mig -> m (Set (PgCompName, Bool))
  customMigration :: PgTableName -> Set PgCompName -> MigExt mig -> m ()

instance CustomMigration m ('Mig from to m Void) where
  customTypeKeys = \case
  customMigration _ _ = \case

class HoistMigration m n ext ext' | m n ext -> ext' where
  hoistMigration :: (∀ x . m x -> n x) -> ext -> ext'

instance HoistMigration m n Void Void where
  hoistMigration _ = \case

class HoistMigrations m n migs migs' | m n migs -> migs' where
  hoistMigrations :: (∀ x . m x -> n x) -> Migrations m migs -> Migrations n migs'

instance HoistMigrations m n '[] '[] where
  hoistMigrations _ Migrations {..} = Migrations {..}

instance (
    HoistMigration m n ext ext',
    HoistMigrations m n migs migs'
  ) => HoistMigrations m n ('Mig from to m ext : migs) ('Mig from to n ext' : migs') where
  hoistMigrations f (Migrations (Migration {..} :* migs)) =
    Migrations (Migration {actions = hoistAction actions, ..} :* unMigrations (hoistMigrations f (Migrations migs)))
    where
      hoistAction = \case
        CustomActions ext -> CustomActions (hoistMigration f ext)
        AutoActions {..} -> AutoActions {..}
