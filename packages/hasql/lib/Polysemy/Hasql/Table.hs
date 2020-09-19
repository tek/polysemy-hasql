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
import qualified Polysemy.Db.Data.TableStructure as TableStructure (TableStructure(_name))
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

columnsStatement :: Statement Text [(Text, Text)]
columnsStatement =
  Statement.query code decoder encoder
  where
    code =
      SqlCode [qt|select "column_name", "data_type" from information_schema.columns where "table_name" = $1|]
    decoder =
      tuple text text
    text =
      Decoders.column (Decoders.nonNullable Decoders.text)
    encoder =
      Encoders.param (Encoders.nonNullable Encoders.text)

tableColumns ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  TableName ->
  Sem r (Maybe (NonEmpty Column))
tableColumns connection (TableName tableName) =
  nonEmpty . fmap cons <$> runStatement connection tableName columnsStatement
  where
    cons (name, dataType) =
      Column name dataType def Nothing

createTable ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  TableStructure ->
  Sem r ()
createTable connection struct@(TableStructure _ columns) = do
  traverse_ createType types
  run (Statement.createTable struct)
  where
    run =
      runStatement connection ()
    types =
      catMaybes (Column.customType <$> toList columns)
    createType ct@(CompositeType name _ cols) = do
      run (Statement.dropType name)
      traverse_ (run . Statement.dropType . TableStructure._name) cols
      traverse_ (run . Statement.createCtorType) cols
      run (Statement.createSumType ct)

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
