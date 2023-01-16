module Sqel.Data.Migration where

import Data.Some (Some)
import Generics.SOP (NP (Nil, (:*)), SListI, hmap)
import Hasql.Encoders (Params)

import Sqel.Data.Dd ((:>) ((:>)))
import Sqel.Data.PgType (ColumnType, PgColumnName, PgTable)
import Sqel.Data.PgTypeName (PgTypeName)

data MigrationTypeAction where
  AddColumn :: PgColumnName -> ColumnType -> Maybe (a, Params a) -> MigrationTypeAction
  RemoveColumn :: PgColumnName -> ColumnType -> MigrationTypeAction
  RenameColumn :: PgColumnName -> PgColumnName -> MigrationTypeAction

data MigrationAction =
  ModifyType Bool (Some PgTypeName) [MigrationTypeAction]

data Mig =
  Mig {
    from :: Type,
    to :: Type
  }

type Migration :: (Type -> Type) -> Mig -> Type
data Migration m t where
  Migration :: {
    tableFrom :: PgTable from,
    tableTo :: PgTable to,
    changes :: [MigrationAction],
    migration :: m ()
  } -> Migration m ('Mig from to)

hoistMigration :: (∀ x . m x -> n x) -> Migration m t -> Migration n t
hoistMigration f Migration {..} =
  Migration {migration = f migration, ..}

type MigList :: [Type] -> [Mig]
type family MigList as where
  MigList '[] = '[]
  MigList [new, old] = '[ 'Mig old new]
  MigList (new : old : as) = 'Mig old new : MigList (old : as)

type Migs :: [Type] -> Type -> [Mig]
type family Migs old cur where
  Migs '[] _ = '[]
  Migs '[o] cur = '[ 'Mig o cur]
  Migs (o : os) cur = 'Mig o cur : MigList (o : os)

type Migrations :: (Type -> Type) -> [Type] -> Type -> Type
newtype Migrations m old cur =
  Migrations { unMigrations :: NP (Migration m) (Migs old cur) }

hoistMigrations ::
  SListI (Migs old cur) =>
  (∀ x . m x -> n x) ->
  Migrations m old cur ->
  Migrations n old cur
hoistMigrations f (Migrations np) =
  Migrations (hmap (hoistMigration f) np)

class MkMigrations arg m migs | arg -> m migs, m migs -> arg where
  mkMigrations :: arg -> NP (Migration m) migs

instance (
    MkMigrations old m (mig1 : migs)
  ) => MkMigrations (Migration m ('Mig from to) :> old) m ('Mig from to : mig1 : migs) where
    mkMigrations (next :> old) =
      next :* mkMigrations old

instance MkMigrations (Migration m ('Mig from to)) m '[ 'Mig from to] where
  mkMigrations next =
    next :* Nil

migrate ::
  MkMigrations arg m (Migs old cur) =>
  arg ->
  Migrations m old cur
migrate =
  Migrations . mkMigrations

noMigrations :: Migrations m '[] a
noMigrations =
  Migrations Nil
