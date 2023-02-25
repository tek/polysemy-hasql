module Sqel.Migration.Metadata where

import qualified Data.Map.Strict as Map
import Exon (exon)
import Prettyprinter (Pretty, pretty, vsep, (<+>))

import qualified Sqel.Class.MigrationEffect as MigrationEffect
import Sqel.Class.MigrationEffect (MigrationEffect)
import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (
  ColumnType,
  PgColumn (PgColumn),
  PgColumnName (PgColumnName),
  PgPrimName (PgPrimName),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (PgTableName, pattern PgTypeName, PgTypeName)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Statement as Statement
import Sqel.Statement (tableColumnsSql)

newtype DbCols =
  DbCols { unDbCols :: Map PgColumnName (Either PgTypeRef PgPrimName) }
  deriving stock (Eq, Show, Generic)

newtype PrettyColMap =
  PrettyColMap { unPrettyColMap :: DbCols }
  deriving stock (Eq, Show, Generic)

instance Pretty PrettyColMap where
  pretty (PrettyColMap (DbCols cols)) =
    vsep (uncurry col <$> Map.toList cols)
    where
      col name = \case
        Right tpe -> "*" <+> pretty name <+> pretty tpe
        Left ref -> "+" <+> pretty name <+> pretty ref

typeColumns ::
  Monad m =>
  MigrationEffect m =>
  Sql ->
  PgTypeName table ->
  m DbCols
typeColumns code (PgTypeName name) =
  DbCols . Map.fromList . fmap mktype <$> MigrationEffect.runStatement name (Statement.dbColumns code)
  where
    mktype = \case
      (col, "USER-DEFINED", n) ->
        (PgColumnName col, Left (PgTypeRef n))
      (col, n, _) ->
        (PgColumnName col, Right (PgPrimName n))

tableColumns ::
  Monad m =>
  MigrationEffect m =>
  PgTableName ->
  m DbCols
tableColumns =
  typeColumns tableColumnsSql

columnMap :: [PgColumn] -> Map PgColumnName ColumnType
columnMap =
  Map.fromList . fmap \ PgColumn {name, pgType} -> (name, pgType)

logType ::
  MigrationEffect m =>
  Text ->
  DbCols ->
  DbCols ->
  m ()
logType desc dbCols colsByName =
  MigrationEffect.log [exon|Trying #{desc} with:
#{show (pretty (PrettyColMap colsByName))}
for existing #{desc} with
#{show (pretty (PrettyColMap dbCols))}|]

data TypeStatus =
  Absent
  |
  Mismatch
  |
  Match
  deriving stock (Eq, Show, Generic)

typeStatus ::
  DbCols ->
  DbCols ->
  TypeStatus
typeStatus (DbCols dbCols) (DbCols colByName)
  | Map.null dbCols = Absent
  | dbCols == colByName = Match
  | otherwise = Mismatch
