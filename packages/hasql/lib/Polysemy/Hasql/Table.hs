module Polysemy.Hasql.Table where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Hasql.Connection (Connection)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Session (QueryError)
import qualified Hasql.Session as Session (run, statement)
import Hasql.Statement (Statement)

import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.TableName (TableName(TableName))
import qualified Polysemy.Db.Data.TableStructure as Column
import Polysemy.Db.Data.TableStructure (Column(Column), CompositeType(CompositeType), TableStructure(TableStructure))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.TableStructure (GenTableStructure(genTableStructure))

runStatement ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  p ->
  Statement p d ->
  Sem r d
runStatement connection args statement =
  fromEither =<< embed (Session.run (Session.statement args statement) connection)

dbColumnsStatement ::
  SqlCode ->
  Statement Text [(Text, Text)]
dbColumnsStatement sql =
  Statement.query sql decoder encoder
  where
    decoder =
      tuple text text
    text =
      Decoders.column (Decoders.nonNullable Decoders.text)
    encoder =
      Encoders.param (Encoders.nonNullable Encoders.text)

dbColumnsFor ::
  Members [Embed IO, Error QueryError] r =>
  SqlCode ->
  Connection ->
  TableName ->
  Sem r (Maybe (NonEmpty Column))
dbColumnsFor sql connection (TableName tableName) =
  nonEmpty . fmap cons <$> runStatement connection tableName (dbColumnsStatement sql)
  where
    cons (name, dataType) =
      Column name dataType def Nothing

tableColumns ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  TableName ->
  Sem r (Maybe (NonEmpty Column))
tableColumns =
  dbColumnsFor code
  where
    code =
      SqlCode [qt|select "column_name", "data_type" from information_schema.columns where "table_name" = $1|]

typeColumns ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  TableName ->
  Sem r (Maybe (NonEmpty Column))
typeColumns =
  dbColumnsFor code
  where
    code =
      SqlCode [qt|select "attribute_name", "data_type" from information_schema.attributes where "udt_name" = $1|]

-- TODO
updateType ::
  NonEmpty Column ->
  Sem r ()
updateType _ =
  unit

initCtorType ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  TableStructure ->
  Sem r ()
initCtorType connection col@(TableStructure name _) = do
  exists <- isJust <$> typeColumns connection name
  unless exists (run (Statement.createCtorType col))
  where
    run =
      runStatement connection ()

initType ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  CompositeType ->
  Sem r ()
initType connection tpe@(CompositeType name _ cols) = do
  existing <- typeColumns connection name
  maybe createType updateType existing
  where
    createType = do
      traverse_ (initCtorType connection) cols
      run (Statement.createSumType tpe)
    run =
      runStatement connection ()

createTable ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  TableStructure ->
  Sem r ()
createTable connection struct@(TableStructure _ columns) = do
  traverse_ (initType connection) types
  run (Statement.createTable struct)
  where
    run =
      runStatement connection ()
    types =
      mapMaybe Column.customType (toList columns)

dropTable ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  TableName ->
  Sem r ()
dropTable connection =
  runStatement connection () . Statement.dropTable

columnMap ::
  Foldable t =>
  t Column ->
  Map Text Text
columnMap cols =
  Map.fromList (columnTuple <$> toList cols)
  where
    columnTuple (Column k v _ _) =
      (k, v)

missingColumns ::
  [Column] ->
  [Column] ->
  Maybe (NonEmpty Column)
missingColumns dbColumns dataColumns =
  nonEmpty $ filter missingColumn dataColumns
  where
    missingColumn (Column name _ _ _) =
      not (Map.member name dbMap)
    dbMap =
      columnMap dbColumns

sameType :: Text -> Text -> Bool
sameType "ARRAY" dataType =
  Text.takeEnd 2 dataType == "[]"
sameType dbType dataType =
  dbType == dataType

mismatchedColumns ::
  [Column] ->
  [Column] ->
  Maybe (NonEmpty (Text, Column))
mismatchedColumns dbColumns dataColumns =
  nonEmpty $ catMaybes (mismatchedColumn <$> toList dataColumns)
  where
    mismatchedColumn dataColumn@(Column name dataType _ comp) = do
      dbType <- Map.lookup (fromMaybe name ("USER-DEFINED" <$ comp)) dbMap
      if sameType dbType dataType then Nothing else Just (dbType, dataColumn)
    dbMap =
      columnMap dbColumns

reportMismatchedColumns ::
  Member (Error DbError) r =>
  TableName ->
  NonEmpty (Text, Column) ->
  Sem r ()
reportMismatchedColumns (TableName name) columns =
  throw (DbError.Table [qt|mismatched columns in table `#{name}`: #{columnsDescription}|])
  where
    columnsDescription =
      Text.intercalate ";" (toList (columnDescription <$> columns))
    columnDescription (dbType, Column colName dataType _ _) =
      [qt|db: #{colName} :: #{dbType}, app: #{colName} :: #{dataType}|]

updateTable ::
  Members [Embed IO, Error DbError] r =>
  NonEmpty Column ->
  Connection ->
  TableStructure ->
  Sem r ()
updateTable (toList -> existing) connection (TableStructure name target) = do
  mapError (DbError.Query . show) $ traverse_ alter missing
  traverse_ (reportMismatchedColumns name) (mismatchedColumns existing target)
  where
    alter =
      runStatement connection () . Statement.alter name
    missing =
      missingColumns existing target

initTable ::
  Members [Embed IO, Error DbError] r =>
  Connection ->
  TableStructure ->
  Sem r ()
initTable connection t@(TableStructure name _) =
  process =<< liftError (tableColumns connection name)
  where
    process (Just existing) =
      updateTable existing connection t
    process Nothing =
      liftError $ createTable connection t
    liftError =
      mapError (DbError.Query . show)

initTableGen ::
  ∀ d rep r .
  Members [Embed IO, Error DbError] r =>
  GenTableStructure d rep =>
  Connection ->
  Sem r ()
initTableGen connection = do
  initTable connection (genTableStructure @d @rep)

initTableE ::
  ∀ d rep r .
  Member (Embed IO) r =>
  GenTableStructure d rep =>
  Connection ->
  Sem r (Either DbError ())
initTableE =
  runError . initTableGen @d @rep
