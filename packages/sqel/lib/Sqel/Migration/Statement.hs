module Sqel.Migration.Statement where

import qualified Control.Monad.Trans.Writer.Strict as Mtl
import qualified Data.Map.Strict as Map
import qualified Exon
import Hasql.Encoders (Params)
import qualified Hasql.Session as Session
import Hasql.Session (Session)
import qualified Text.Show as Show

import qualified Sqel.Data.Migration as Migration
import Sqel.Data.Migration (
  ColumnAction (AddColumn, RemoveColumn, RenameColumn, RenameColumnType),
  MigrationActions (AutoActions, CustomActions),
  TypeAction (AddAction, ModifyAction, RenameAction),
  )
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumnName (PgColumnName),
  PgComposite (PgComposite),
  PgPrimName (PgPrimName),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (
  pattern PgCompName,
  PgTableName,
  pattern PgTableName,
  pattern PgTypeName,
  PgTypeName,
  pgTableName,
  )
import Sqel.Data.Sql (Sql (Sql), sql)
import qualified Sqel.Sql.Type as Sql
import Sqel.Statement (unprepared)

data MigrationStatement where
  MigrationStatement :: p -> Params p -> Sql -> MigrationStatement

instance Show MigrationStatement where
  show (MigrationStatement _ _ s) = show s

migrationStatementSql :: MigrationStatement -> Sql
migrationStatementSql (MigrationStatement _ _ s) =
  s

alterStatement ::
  PgTypeName table ->
  p ->
  Params p ->
  (Sql -> Sql -> Sql) ->
  Mtl.Writer [MigrationStatement] ()
alterStatement typeName p enc f =
  Mtl.tell [MigrationStatement p enc (f [sql|alter #{entity} ##{pgTableName name}|] attr)]
  where
    (entity, attr, name) = case typeName of
      PgTableName n -> ("table", "column", n)
      PgCompName n -> ("type", "attribute", n)

-- TODO maybe the default value can be null and the encoder Maybe, to unify the cases
columnStatements' ::
  PgTypeName table ->
  ColumnAction ->
  Mtl.Writer [MigrationStatement] ()
columnStatements' typeName = \case
  AddColumn (PgColumnName colName) tpe md -> do
    alter_ \ alter attr -> [sql|#{alter} add #{attr} ##{colName} #{colTypeName}|]
    case typeName of
      PgTableName _ -> do
        for_ md \ (defVal, enc) -> do
          alterStatement typeName defVal enc \ _ _ -> [sql|update ##{comp} set ##{colName} = $1|]
        for_ (nonEmpty optFrag) \ opt ->
          alter_ \ alter attr ->
            [sql|#{alter} alter #{attr} ##{colName} set #{Exon.intercalate " " opt}|]
      PgCompName _ -> unit
    where
      (optFrag, colTypeName) = case tpe of
        ColumnPrim (PgPrimName n) _ opt -> (opt, Sql n)
        ColumnComp (PgTypeRef n) _ opt -> (opt, Sql n)
  RemoveColumn (PgColumnName name) _ ->
    alter_ \ alter attr -> [sql|#{alter} drop #{attr} ##{name}|]
  RenameColumn (PgColumnName old) (PgColumnName new) ->
    alter_ \ alter attr -> [sql|#{alter} rename #{attr} ##{old} to ##{new}|]
  RenameColumnType (PgColumnName old) (PgTypeName new) ->
    alter_ \ alter attr -> [sql|#{alter} alter #{attr} ##{old} set data type ##{new}|]
  where
    alter_ = alterStatement typeName () mempty
    PgTypeName comp = typeName

typeActionStatements :: PgTypeName table -> TypeAction table -> Mtl.Writer [MigrationStatement] ()
typeActionStatements typeName = \case
  ModifyAction _ cols ->
    traverse_ (columnStatements' typeName) cols
  RenameAction newName@(PgTypeName new) cols -> do
    alterStatement typeName () mempty \ alter _ -> [sql|#{alter} rename to ##{new}|]
    traverse_ (columnStatements' newName) cols
  AddAction cols ->
    Mtl.tell [MigrationStatement () mempty (Sql.createProdType (PgComposite typeName cols))]

typeStatements :: PgTypeName table -> TypeAction table -> [MigrationStatement]
typeStatements name =
  snd .
  runIdentity .
  Mtl.runWriterT .
  typeActionStatements name

migrationStatements :: PgTableName -> MigrationActions ext -> [MigrationStatement]
migrationStatements tableName = \case
  AutoActions {..} ->
    typeStatements tableName table <> (Map.toList types >>= \ (name, actions) -> typeStatements name actions)
  CustomActions _ ->
    []

migrationSession :: [MigrationStatement] -> Session ()
migrationSession =
  traverse_ \ (MigrationStatement p enc stmt) -> Session.statement p (unprepared @() stmt unit enc)
