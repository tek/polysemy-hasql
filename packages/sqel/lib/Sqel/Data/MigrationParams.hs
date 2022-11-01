module Sqel.Data.MigrationParams where

newtype MigrationDefault a =
  MigrationDefault { unMigrationDefault :: a }
  deriving stock (Eq, Show, Generic)

type MigrationRename :: Symbol -> Type
data MigrationRename name =
  MigrationRename
  deriving stock (Eq, Show, Generic)

type family MigrationRenameK (ps :: [Type]) :: Maybe Symbol where
  MigrationRenameK '[] = 'Nothing
  MigrationRenameK (MigrationRename name : _) = 'Just name
  MigrationRenameK (_ : ps) = MigrationRenameK ps

data MigrationDelete =
  MigrationDelete
  deriving stock (Eq, Show, Generic)

type family MigrationDeleteK (ps :: [Type]) :: Bool where
  MigrationDeleteK '[] = 'False
  MigrationDeleteK (MigrationDelete : _) = 'True
  MigrationDeleteK (_ : ps) = MigrationDeleteK ps
