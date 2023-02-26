module Sqel.Migration.Run where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Exon (exon)
import Generics.SOP (All, NP (Nil, (:*)))

import qualified Sqel.Class.MigrationEffect as MigrationEffect
import Sqel.Class.MigrationEffect (MigrationEffect (runMigrationStatements))
import Sqel.Data.Migration (
  CompAction,
  CustomMigration (customMigration, customTypeKeys),
  MigExt,
  Migration (Migration),
  MigrationActions (AutoActions, CustomActions),
  Migrations (Migrations),
  TypeAction (AddAction),
  )
import Sqel.Data.PgType (
  ColumnType (ColumnComp, ColumnPrim),
  PgColumns (PgColumns),
  PgComposite (PgComposite),
  PgTable (PgTable),
  )
import Sqel.Data.PgTypeName (PgCompName, PgTableName, pattern PgTableName, pattern PgTypeName, PgTypeName, getPgTypeName)
import Sqel.Data.Sql (Sql)
import Sqel.Migration.Init (initTable)
import Sqel.Migration.Metadata (
  DbCols (DbCols),
  TypeStatus (Absent, Match, Mismatch),
  columnMap,
  logType,
  typeColumns,
  typeStatus,
  )
import Sqel.Migration.Statement (typeStatements)
import Sqel.Statement (tableColumnsSql, typeColumnsSql)

typeMatchWith ::
  Monad m =>
  MigrationEffect m =>
  Text ->
  PgTypeName table ->
  PgColumns ->
  Sql ->
  m TypeStatus
typeMatchWith desc name (PgColumns cols) code = do
  dbCols <- typeColumns code name
  logType desc dbCols colsByName
  pure (typeStatus dbCols colsByName)
  where
    colsByName = DbCols $ columnMap cols <&> \case
      ColumnPrim n _ _ -> Right n
      ColumnComp n _ _ -> Left n

typeMatch ::
  Monad m =>
  MigrationEffect m =>
  PgComposite ->
  m TypeStatus
typeMatch (PgComposite name cols) =
  typeMatchWith "type" name cols typeColumnsSql

tableMatch ::
  Monad m =>
  MigrationEffect m =>
  TypeStatus ->
  PgTable a ->
  m TypeStatus
tableMatch Absent _ =
  pure Absent
tableMatch _ (PgTable name cols _ _ _ _) =
  typeMatchWith "table" name cols tableColumnsSql

matches ::
  Monad m =>
  MigrationEffect m =>
  TypeStatus ->
  PgTable from ->
  m (TypeStatus, Set PgCompName)
matches initialStatus table = do
  pairA (tableMatch initialStatus table) (foldM folder Set.empty (table ^. #types))
  where
    folder acc t =
      typeMatch t <&> \case
        Match -> Set.insert (t ^. #name) acc
        _ -> acc

runAction ::
  MigrationEffect m =>
  PgTypeName table ->
  TypeAction table ->
  m ()
runAction typeName action =
  runMigrationStatements (typeStatements typeName action)

-- TODO topo sort the types
runTypesMigration ::
  Monad m =>
  MigrationEffect m =>
  Set PgCompName ->
  Map PgCompName CompAction ->
  m ()
runTypesMigration eligible actions =
  for_ (Map.toList (Map.restrictKeys actions eligible)) \ (name, tpe) ->
    runAction name tpe

-- TODO shouldn't the types be migrated first?
runMigration ::
  ∀ mig m .
  Monad m =>
  MigrationEffect m =>
  CustomMigration m mig =>
  TypeStatus ->
  PgTableName ->
  Set PgCompName ->
  MigrationActions (MigExt mig) ->
  m ()
runMigration status tableName eligible = \case
  AutoActions tableAction typeActions -> do
    MigrationEffect.log "Starting migration"
    when (status == Match) (runAction tableName tableAction)
    runTypesMigration eligible typeActions
  CustomActions actions ->
    customMigration @m @mig tableName eligible actions

tryRunMigration ::
  ∀ mig m .
  Monad m =>
  MigrationEffect m =>
  CustomMigration m mig =>
  TypeStatus ->
  PgTableName ->
  Set PgCompName ->
  MigrationActions (MigExt mig) ->
  m ()
tryRunMigration Mismatch (PgTableName name) _ _ =
  MigrationEffect.error [exon|No migration fits the current table layout for #{name}|]
tryRunMigration status tableName eligible actions =
  runMigration @mig status tableName eligible actions

autoKeys ::
  Map PgCompName CompAction ->
  Set (PgCompName, Bool)
autoKeys typeActions =
  Set.fromList (Map.elems (Map.mapWithKey keyAndAddition typeActions))
  where
    keyAndAddition k = \case
      AddAction _ -> (k, True)
      _ -> (k, False)

typeKeys ::
  ∀ mig m .
  Applicative m =>
  CustomMigration m mig =>
  MigrationActions (MigExt mig) ->
  m (Set (PgCompName, Bool))
typeKeys = \case
  AutoActions _ typeActions ->
    pure (autoKeys typeActions)
  CustomActions actions ->
    customTypeKeys @m @mig actions

collectDirectMatches :: Set (PgCompName, Bool) -> Set PgCompName -> Set PgCompName
collectDirectMatches actions curMatches =
  Set.fromList (fst <$> filter (uncurry matchAction) (Set.toList actions))
  where
    matchAction name = \case
      True -> not (Set.member name curMatches)
      False -> Set.member name curMatches

matchMessage :: PgTypeName table -> TypeStatus -> Set PgCompName -> Set PgCompName -> Set PgCompName -> Text
matchMessage (PgTypeName tableName) status currentMatches directMatches allMatches =
  [exon|Table #{tableName}: #{show status}
Matching types: #{showNames currentMatches}
Direct action matches: #{showNames directMatches}
All action matches: #{showNames allMatches}
|]
  where
    showNames =
      Text.intercalate ", " .
      fmap (\ (PgTypeName name) -> name) .
      Set.toList

runMigrationSteps ::
  ∀ m migs a .
  Monad m =>
  MigrationEffect m =>
  All (CustomMigration m) migs =>
  TypeStatus ->
  Set PgCompName ->
  PgTable a ->
  NP Migration migs ->
  m (TypeStatus, Set PgCompName)
runMigrationSteps initialStatus _ _ Nil =
  pure (initialStatus, mempty)
runMigrationSteps initialStatus laterMatches table ((Migration currentTable _ actions :: Migration mig) :* t) = do
  -- types that are identical in the database and the current migration's from-table
  (status, currentTypeMatches) <- matches initialStatus currentTable
  actionNamesAndAdditions <- typeKeys @mig @m actions
  let
    actionNames = Set.fromList (fst <$> Set.toList actionNamesAndAdditions)
    mismatchHere = status == Mismatch
    -- actions whose types match the database before any migrations are executed.
    -- these cannot be additions, since they are absent from the database if they are applicable.
    -- check whether additions need special treatment, i.e. execute if absent.
    directMatches = collectDirectMatches actionNamesAndAdditions currentTypeMatches
    -- actions whose types either match this migration's from-table or that of a later migration.
    allMatches = Set.union directMatches laterMatches
  MigrationEffect.log (matchMessage (currentTable ^. #name) status currentTypeMatches directMatches allMatches)
  (newStatus, eligible) <-
    -- if actionNames is a subset of allMatches, all actions can be executed either here or in a later migration.
    -- therefore we don't need to check earlier migrations and just execute the direct matches here and relay the rest
    -- to later migrations.
    -- if the current migration's table doesn't match the existing table, we still have to run earlier migrations,
    -- but those don't have to run any type actions.
    -- if the table is absent, earlier migrations don't have to be run, just like a match.
    if not mismatchHere && Set.isSubsetOf actionNames allMatches
    then pure (status, directMatches)
    else do
      -- if the table matched in an earlier migration, it will match here as well since the earlier migration
      -- executed.
      -- same for types, so add earlier matches to the direct matches.
      (earlierStatus, earlierMatches) <- runMigrationSteps Mismatch allMatches table t
      pure (earlierStatus, Set.union earlierMatches directMatches)
  runMigration @mig newStatus (table ^. #name) eligible actions
  pure (Match, eligible)

createAbsent ::
  Monad m =>
  MigrationEffect m =>
  PgTable a ->
  TypeStatus ->
  m ()
createAbsent table = \case
  Absent -> initTable table
  _ -> unit

runMigrations ::
  ∀ m migs a .
  Monad m =>
  MigrationEffect m =>
  All (CustomMigration m) migs =>
  PgTable a ->
  Migrations m migs ->
  m ()
runMigrations table (Migrations steps) = do
  MigrationEffect.log [exon|Checking migrations for '#{getPgTypeName (table ^. #name)}'|]
  initialStatus <- tableMatch Mismatch table
  (status, _) <- runMigrationSteps initialStatus mempty table steps
  createAbsent table status
