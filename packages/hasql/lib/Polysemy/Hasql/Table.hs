module Polysemy.Hasql.Table where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Exon (exon)
import Hasql.Connection (Connection)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Session (QueryError)
import qualified Hasql.Session as Session (run, statement)
import Hasql.Statement (Statement)
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Log as Log

import qualified Polysemy.Hasql.Data.DbType as Data
import Polysemy.Hasql.Data.DbType (Column (Column), Name (Name), TypeName (CompositeTypeName, PrimTypeName), unName)
import qualified Polysemy.Hasql.Data.ExistingColumn as ExistingColumn
import Polysemy.Hasql.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Polysemy.Hasql.Data.SqlCode (SqlCode (unSqlCode), esql)
import Polysemy.Hasql.DbType (baseColumns, typeName)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.DataColumn (TableStructure, tableStructure)

runStatement ::
  Members [Embed IO, Stop QueryError] r =>
  Connection ->
  p ->
  Statement p d ->
  Sem r d
runStatement connection args statement =
  stopEither =<< embed (Session.run (Session.statement args statement) connection)

dbColumnsStatement ::
  SqlCode ->
  Statement Text [(Text, Text)]
dbColumnsStatement sql =
  Statement.prepared sql decoder encoder
  where
    decoder =
      (,) <$> text' <*> text'
    text' =
      Decoders.column (Decoders.nonNullable Decoders.text)
    encoder =
      Encoders.param (Encoders.nonNullable Encoders.text)

dbColumnsFor ::
  Members [Embed IO, Stop QueryError] r =>
  SqlCode ->
  Connection ->
  Name ->
  Sem r (Maybe (NonEmpty ExistingColumn))
dbColumnsFor sql connection (Name tableName) =
  nonEmpty . fmap cons <$> runStatement connection tableName (dbColumnsStatement sql)
  where
    cons (name, dataType) =
      ExistingColumn (Name name) dataType

tableColumns ::
  Members [Embed IO, Stop QueryError] r =>
  Connection ->
  Name ->
  Sem r (Maybe (NonEmpty ExistingColumn))
tableColumns =
  dbColumnsFor code
  where
    code =
      [esql|select "column_name", "data_type" from information_schema.columns where "table_name" = $1|]

typeColumns ::
  Members [Embed IO, Stop QueryError] r =>
  Connection ->
  TypeName ->
  Sem r (Maybe (NonEmpty ExistingColumn))
typeColumns connection =
  dbColumnsFor code connection . Name . unSqlCode . typeName
  where
    code =
      [esql|select "attribute_name", "data_type" from information_schema.attributes where "udt_name" = $1|]

-- TODO
updateType ::
  NonEmpty ExistingColumn ->
  Sem r ()
updateType _ =
  unit

initCtorType ::
  Members [Embed IO, Stop QueryError] r =>
  Connection ->
  Column ->
  Sem r ()
initCtorType connection col@(Column _ _ tpe _ _) = do
  exists <- isJust <$> typeColumns connection tpe
  unless exists (runStatement connection () (Statement.createCtorType col))

initProd ::
  Members [Embed IO, Stop QueryError] r =>
  Connection ->
  TypeName ->
  [Column] ->
  Sem r ()
initProd connection tpe columns = do
  existing <- typeColumns connection tpe
  maybe createType updateType existing
  where
    createType = do
      traverse_ (initType connection) columns
      run (Statement.createProdType tpe columns)
    run =
      runStatement connection ()

-- TODO use effects for running statements
-- TODO fix undefined
initType ::
  Members [Embed IO, Stop QueryError] r =>
  Connection ->
  Data.Column ->
  Sem r ()
initType connection (Column _ _ tpe _ dbType) =
  case dbType of
    Data.Prim ->
      unit
    Data.Prod columns -> do
      initProd connection tpe columns
    Data.Sum (Column _ _ _ _ (Data.Prod columns)) ->
      initProd connection tpe columns
    _ ->
      undefined

createTable ::
  Members [Embed IO, Stop QueryError] r =>
  Connection ->
  Column ->
  Sem r ()
createTable connection table = do
  traverse_ (initType connection) (baseColumns table)
  run (Statement.createTable table)
  where
    run =
      runStatement connection ()

dropTable ::
  Members [Embed IO, Stop QueryError] r =>
  Connection ->
  Name ->
  Sem r ()
dropTable connection =
  runStatement connection () . Statement.dropTable

columnMap ::
  Foldable t =>
  t ExistingColumn ->
  Map Name Text
columnMap cols =
  Map.fromList (columnTuple <$> toList cols)
  where
    columnTuple ExistingColumn {..} =
      (name, ctype)

missingColumns ::
  [ExistingColumn] ->
  [Column] ->
  Maybe (NonEmpty Column)
missingColumns dbColumns dataColumns =
  nonEmpty $ filter missingColumn dataColumns
  where
    missingColumn (Column name _ _ _ _) =
      not (Map.member name dbMap)
    dbMap =
      columnMap dbColumns

sameType :: Text -> TypeName -> Bool
sameType "ARRAY" (PrimTypeName dataType) =
  Text.takeEnd 2 dataType == "[]"
sameType dbType (PrimTypeName dataType) =
  dbType == dataType
sameType _ (CompositeTypeName _) =
  False

userDefined :: Text
userDefined =
  "USER-DEFINED"

mismatchedColumns ::
  [ExistingColumn] ->
  [Column] ->
  Maybe (NonEmpty (Name, Column))
mismatchedColumns dbColumns dataColumns =
  nonEmpty (mapMaybe mismatchedColumn (toList dataColumns))
  where
    mismatchedColumn dataColumn@(Column name _ tpe _ _) = do
      -- dbType <- Map.lookup (fromMaybe name ("USER-DEFINED" <$ comp)) dbMap
      dbType <- Map.lookup name dbMap
      if sameType dbType tpe then Nothing else Just (Name dbType, dataColumn)
    dbMap =
      columnMap dbColumns

reportMismatchedColumns ::
  Member (Stop DbError) r =>
  Name ->
  NonEmpty (Name, Column) ->
  Sem r ()
reportMismatchedColumns (Name name) columns =
  stop (DbError.Table [exon|mismatched columns in table `#{name}`: #{columnsDescription}|])
  where
    columnsDescription =
      Text.intercalate ";" (toList (columnDescription <$> columns))
    columnDescription (dbType, Column colName _ tpe _ _) =
      [exon|db: #{show colName} :: #{show dbType}, app: #{show colName} :: #{show tpe}|]

updateTable ::
  Members [Embed IO, Stop DbError] r =>
  NonEmpty ExistingColumn ->
  Connection ->
  Column ->
  Sem r ()
updateTable (toList -> existing) connection column@(Column name selector _ _ _) = do
  mapStop (DbError.Query . show) $ traverse_ alter missing
  traverse_ (reportMismatchedColumns name) (mismatchedColumns existing target)
  where
    alter =
      runStatement connection () . Statement.alter selector
    missing =
      missingColumns existing target
    target =
      baseColumns column

initTable ::
  Members [Log, Stop DbError, Embed IO] r =>
  Connection ->
  Column ->
  Sem r ()
initTable connection t@(Column name _ _ _ _) = do
  Log.debug [exon|initializing table `#{unName name}`|]
  process =<< liftError (tableColumns connection name)
  where
    process (Just existing) =
      updateTable existing connection t
    process Nothing =
      liftError (createTable connection t)
    liftError =
      mapStop (DbError.Query . show)

initTableGen ::
  ∀ d rep r .
  Members [Log, Stop DbError, Embed IO] r =>
  TableStructure d rep =>
  Connection ->
  Sem r ()
initTableGen connection = do
  initTable connection (tableStructure @d @rep)

initTableE ::
  ∀ d rep r .
  Members [Log, Embed IO] r =>
  TableStructure d rep =>
  Connection ->
  Sem r (Either DbError ())
initTableE =
  runStop . initTableGen @d @rep
