module Sqel.Migration.Consistency where

import qualified Control.Exception as Base
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, throwE, withExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LByteString
import Data.List.Extra (zipWithLongest)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Generics.SOP (NP (Nil, (:*)))
import Path (Abs, Dir, File, Path, parseRelFile, toFilePath, (</>))
import Path.IO (createDirIfMissing, doesFileExist)
import Prelude hiding (tryIO)
import System.IO.Error (IOError)

import Sqel.Data.Migration (Migration (Migration), Migrations (Migrations), changes, tableFrom, tableTo)
import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumnName,
  PgColumns (PgColumns),
  PgComposite (PgComposite),
  PgPrimName (PgPrimName),
  PgTable (PgTable),
  PgTypeRef (PgTypeRef),
  )
import Sqel.Data.PgTypeName (PgTableName, pattern PgTypeName)
import Sqel.Data.Sql (Sql)
import Sqel.Migration.Statement (migrationStatementSql, migrationStatements)
import qualified Sqel.Sql.Type as Sql
import Sqel.Text.Quote (squote)

tryIO :: MonadIO m => IO a -> m (Either Text a)
tryIO =
  liftIO . fmap (first show) . Base.try @IOError

data MigrationMetadata =
  MigrationMetadata {
    name :: PgTableName,
    table :: PgColumns,
    types :: [PgComposite],
    statementsTable :: [Sql],
    statementsMigration :: [Sql]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

tableStatements :: PgTable a -> [Sql]
tableStatements table =
  Sql.createTable table : (Sql.createProdType <$> types)
  where
    types = snd <$> Map.toAscList (table ^. #types)

tableMetadata :: PgTable a -> MigrationMetadata
tableMetadata table =
  MigrationMetadata {
    name = table ^. #name,
    table = table ^. #columns,
    types,
    statementsTable = tableStatements table,
    statementsMigration = []
  }
  where
    types = snd <$> Map.toAscList (table ^. #types)

migrationMetadata :: Migration m mig -> MigrationMetadata
migrationMetadata Migration {tableFrom, changes} =
  tableMetadata tableFrom & #statementsMigration .~ (migrationStatementSql <$> migrationStatements changes)

currentMetadata :: Migration m mig -> MigrationMetadata
currentMetadata Migration {tableTo} =
  tableMetadata tableTo

migrationMetadatas :: NP (Migration m) migs -> [MigrationMetadata]
migrationMetadatas = \case
  Nil -> []
  m :* ms -> migrationMetadata m : migrationMetadatas ms

headMigrationMetadata :: NP (Migration m) migs -> Maybe MigrationMetadata
headMigrationMetadata = \case
  Nil -> Nothing
  mig :* _ -> Just (currentMetadata mig)

migrationsMetadata :: Migrations m old cur -> [MigrationMetadata]
migrationsMetadata (Migrations migs) =
  reverse (maybeToList (headMigrationMetadata migs) <> migrationMetadatas migs)

jsonFile :: PgTable a -> String
jsonFile PgTable {name = PgTypeName name} =
  [exon|##{name}.json|]

jsonPath ::
  Monad m =>
  Path Abs Dir ->
  PgTable a ->
  ExceptT Text m (Path Abs File)
jsonPath dir table = do
  name <- ExceptT (pure (first pathError (parseRelFile (jsonFile table))))
  pure (dir </> name)
  where
    pathError _ = [exon|Table name couldn't be converted to a path: #{toText tname}|]
    tname = jsonFile table

writeMigrationMetadata ::
  MonadIO m =>
  Path Abs Dir ->
  Migrations n old cur ->
  ExceptT Text m ()
writeMigrationMetadata dir migs@(Migrations (Migration {tableFrom} :* _)) = do
  path <- jsonPath dir tableFrom
  let
    write = LByteString.writeFile (toFilePath path) (Aeson.encode (migrationsMetadata migs))
    writeError e = [exon|Couldn't write migration metadata to '#{show path}': #{e}|]
  ExceptT (first writeError <$> tryIO (createDirIfMissing True dir))
  ExceptT (first writeError <$> tryIO write)
writeMigrationMetadata _ (Migrations Nil) =
  unit

readError :: Path Abs File -> Text -> Text
readError path e =
  [exon|Couldn't read migration metadata from #{show path}: #{e}|]

decodeError :: Path Abs File -> String -> Text
decodeError path e =
  [exon|Migration metadata in '#{show path}' has invalid json format: ##{e}|]

readMigrationMetadata ::
  MonadIO m =>
  Path Abs Dir ->
  Migrations n old cur ->
  ExceptT Text m (Maybe [MigrationMetadata])
readMigrationMetadata dir (Migrations (Migration {tableFrom} :* _)) = do
  path <- jsonPath dir tableFrom
  liftIO (fromRight False <$> tryIO (doesFileExist path)) >>= \case
    False ->
      pure Nothing
    True -> do
      j <- ExceptT (first (readError path) <$> tryIO (ByteString.readFile (toFilePath path)))
      ExceptT (pure (first (decodeError path) (Aeson.eitherDecodeStrict' j)))
readMigrationMetadata _ (Migrations Nil) =
  throwE "Cannot test empty migrations"

indent ::
  Functor t =>
  t Text ->
  t Text
indent =
  fmap (" • " <>)

showType :: ColumnType -> Text
showType =
  squote . \case
    ColumnPrim {name = PgPrimName name} -> name
    ColumnComp (PgTypeRef name) -> name

columnMismatch :: Maybe (PgColumnName, ColumnType) -> Maybe (PgColumnName, ColumnType) -> Text
columnMismatch Nothing (Just (name, tpe)) =
  [exon|A column '##{name}' with type #{showType tpe} was added.|]
columnMismatch (Just (name, tpe)) Nothing =
  [exon|The column '##{name}' with type #{showType tpe} was removed.|]
columnMismatch (Just (gname, gtpe)) (Just (cname, ctpe))
  | gname == cname =
    [exon|The type of the column '##{gname}' was changed from #{showType gtpe} to #{showType ctpe}.|]
  | otherwise =
    [exon|The column '##{gname}' with type #{showType gtpe} was replaced with the column '##{cname}' with type #{showType ctpe}.|]
columnMismatch Nothing Nothing =
  "Internal error"

compareType :: Text -> PgColumns -> PgColumns -> Maybe (NonEmpty Text)
compareType desc (PgColumns golden) (PgColumns current) =
  mismatches <$> nonEmpty (filter (uncurry (/=)) (zipWithLongest (,) golden current))
  where
    mismatches cols = [exon|#{desc} has mismatched columns:|] :| (indent (uncurry columnMismatch <$> toList cols))

compareComp :: Maybe PgComposite -> Maybe PgComposite -> Maybe (NonEmpty Text)
compareComp Nothing Nothing =
  Nothing
compareComp Nothing (Just (PgComposite (PgTypeName name) _)) =
  Just [[exon|The type '#{name}' was added.|]]
compareComp (Just (PgComposite (PgTypeName name) _)) Nothing =
  Just [[exon|The type '#{name}' was removed.|]]
compareComp (Just (PgComposite (PgTypeName gname) gcols)) (Just (PgComposite (PgTypeName cname) ccols))
  | gname == cname =
    compareType [exon|The composite type '#{gname}'|] gcols ccols
  | otherwise =
    Just [[exon|The type '#{gname}' was replaced with a type named '#{cname}'.|]]

compareStep :: MigrationMetadata -> MigrationMetadata -> Maybe (NonEmpty Text)
compareStep golden current =
  join <$> nonEmpty (catMaybes mismatches)
  where
    mismatches =
      compareType [exon|The migration table '#{name}'|] (golden ^. #table) (current ^. #table) :
      zipWithLongest (compareComp) (golden ^. #types) (current ^. #types)
    PgTypeName name = golden ^. #name

checkStep :: Maybe MigrationMetadata -> Maybe MigrationMetadata -> Maybe (NonEmpty Text)
checkStep Nothing _ =
  Nothing
checkStep (Just golden) Nothing =
  let (PgTypeName name) = golden ^. #name
  in Just (pure [exon|A migration for #{name} was removed.|])
checkStep (Just golden) (Just current) =
  compareStep golden current

checkMigrationConsistency :: [MigrationMetadata] -> [MigrationMetadata] -> Either (NonEmpty Text) ()
checkMigrationConsistency golden current =
  maybeToLeft () (join <$> (nonEmpty (catMaybes (zipWithLongest checkStep golden current))))

single ::
  Functor m =>
  ExceptT Text m a ->
  ExceptT (NonEmpty Text) m a
single =
  withExceptT pure

result ::
  Functor m =>
  ExceptT e m () ->
  m (Maybe e)
result =
  runExceptT >>> fmap \case
    Left e -> Just e
    Right () -> Nothing

migrationConsistency ::
  MonadIO m =>
  Path Abs Dir ->
  Migrations n old cur ->
  Bool ->
  m (Maybe (NonEmpty Text))
migrationConsistency dir migs =
  result . \case
    True ->
      single (writeMigrationMetadata dir migs)
    False ->
      single (readMigrationMetadata dir migs) >>= \case
        Just golden ->
          ExceptT (pure (checkMigrationConsistency golden (migrationsMetadata migs)))
        Nothing -> single (writeMigrationMetadata dir migs)
