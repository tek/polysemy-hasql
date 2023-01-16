module Sqel.Migration.Statement where

import qualified Control.Monad.Trans.Writer.Strict as Mtl
import Data.Some (Some, withSome)
import qualified Exon
import Hasql.Encoders (Params)
import qualified Hasql.Session as Session
import Hasql.Session (Session)
import qualified Text.Show as Show

import Sqel.Data.Migration (MigrationAction (ModifyType), MigrationTypeAction (AddColumn, RemoveColumn, RenameColumn))
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumnName (PgColumnName),
  PgPrimName (PgPrimName),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (pattern PgCompName, pattern PgTableName, pattern PgTypeName, PgTypeName)
import Sqel.Data.Sql (Sql (Sql), sql)
import Sqel.Statement (unprepared)

data MigrationStatement where
  MigrationStatement :: p -> Params p -> Sql -> MigrationStatement

instance Show MigrationStatement where
  show (MigrationStatement _ _ s) = show s

migrationStatementSql :: MigrationStatement -> Sql
migrationStatementSql (MigrationStatement _ _ s) =
  s

alterStatement ::
  Some PgTypeName ->
  p ->
  Params p ->
  (Sql -> Sql -> Sql) ->
  Mtl.Writer [MigrationStatement] ()
alterStatement typeName p enc f =
  Mtl.tell [MigrationStatement p enc (f [sql|alter #{entity} ##{Sql name}|] attr)]
  where
    (entity, attr, name) = withSome typeName \case
      PgTableName n -> ("table", "column", n)
      PgCompName n -> ("type", "attribute", n)

-- TODO maybe the default value can be null and the encoder Maybe, to unify the cases
-- TODO use ToSql for these statements
columnStatements ::
  Bool ->
  Some PgTypeName ->
  MigrationTypeAction ->
  Mtl.Writer [MigrationStatement] ()
columnStatements table typeName = \case
  AddColumn (PgColumnName colName) tpe md -> do
    alter_ \ alter attr -> [sql|#{alter} add #{attr} ##{colName} #{colTypeName}|]
    when table do
      for_ md \ (defVal, enc) -> do
        alterStatement typeName defVal enc \ _ _ -> [sql|update ##{comp} set ##{colName} = $1|]
      for_ (nonEmpty optFrag) \ opt ->
        alter_ \ alter attr ->
          [sql|#{alter} alter #{attr} ##{colName} set #{Exon.intercalate " " opt}|]
    where
      (optFrag, colTypeName) = case tpe of
        ColumnPrim (PgPrimName n) _ opt -> (opt, Sql n)
        ColumnComp (PgTypeRef n) -> (mempty, Sql n)
  RenameColumn (PgColumnName old) (PgColumnName new) ->
    alter_ \ alter attr -> [sql|#{alter} rename #{attr} ##{old} to ##{new}|]
  RemoveColumn (PgColumnName name) _ ->
    alter_ \ alter attr -> [sql|#{alter} drop #{attr} ##{name}|]
  where
    alter_ = alterStatement typeName () mempty
    comp = withSome typeName \ (PgTypeName n) -> Sql n

actionStatements :: MigrationAction -> Mtl.Writer [MigrationStatement] ()
actionStatements (ModifyType table name cols) =
  traverse_ (columnStatements table name) cols

migrationStatements :: [MigrationAction] -> [MigrationStatement]
migrationStatements =
  snd .
  runIdentity .
  Mtl.runWriterT .
  traverse_ actionStatements

migrationSession :: [MigrationAction] ->  Session ()
migrationSession =
  traverse_ runS . migrationStatements
  where
    runS (MigrationStatement p enc stmt) = Session.statement p (unprepared @() stmt unit enc)
