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

type MigrationRenameType :: Symbol -> Type
data MigrationRenameType name =
  MigrationRenameType
  deriving stock (Eq, Show, Generic)

type family MigrationRenameTypeK (ps :: [Type]) :: Maybe Symbol where
  MigrationRenameTypeK '[] = 'Nothing
  MigrationRenameTypeK (MigrationRenameType name : _) = 'Just name
  MigrationRenameTypeK (_ : ps) = MigrationRenameTypeK ps

data MigrationDelete =
  MigrationDelete
  deriving stock (Eq, Show, Generic)

type family MigrationDeleteK (ps :: [Type]) :: Bool where
  MigrationDeleteK '[] = 'False
  MigrationDeleteK (MigrationDelete : _) = 'True
  MigrationDeleteK (_ : ps) = MigrationDeleteK ps
