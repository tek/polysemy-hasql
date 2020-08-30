module Polysemy.Hasql.Table where

import Control.Monad (ap)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.Vector (Vector)
import Hasql.Connection (Connection)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Session (QueryError)
import qualified Hasql.Session as Session (run, statement)
import Hasql.Statement (Statement)

import Polysemy.Db.Data.Columns (Column(Column), Columns(Columns))
import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Db.Data.TableName (TableName(TableName))
import Polysemy.Db.Data.TableStructure (TableStructure(TableStructure))
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

columnsStatement :: Statement Text (Vector (Text, Text))
columnsStatement =
  Statement.query code decoder encoder
  where
    code =
      SqlCode [i|select "column_name", "data_type" from information_schema.columns where "table_name" = $1|]
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
tableColumns connection (TableName name) =
  nonEmpty . (`ap` (pure def)) . fmap (uncurry Column) . Vector.toList <$> runStatement connection name columnsStatement

createTable ::
  Members [Embed IO, Error QueryError] r =>
  Connection ->
  TableStructure ->
  Sem r ()
createTable connection =
  runStatement connection () . Statement.createTable

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
    columnTuple (Column k v _) =
      (k, v)

missingColumns ::
  NonEmpty Column ->
  Columns ->
  Maybe (NonEmpty Column)
missingColumns dbColumns (Columns dataColumns) =
  nonEmpty $ filter missingColumn (toList dataColumns)
  where
    missingColumn (Column name _ _) =
      not (Map.member name dbMap)
    dbMap =
      columnMap dbColumns

sameType :: Text -> Text -> Bool
sameType "ARRAY" dataType =
  Text.takeEnd 2 dataType == "[]"
sameType dbType dataType =
  dbType == dataType

mismatchedColumns ::
  NonEmpty Column ->
  Columns ->
  Maybe (NonEmpty (Text, Column))
mismatchedColumns dbColumns (Columns dataColumns) =
  nonEmpty $ catMaybes (mismatchedColumn <$> toList dataColumns)
  where
    mismatchedColumn dataColumn@(Column name dataType _) = do
      dbType <- Map.lookup name dbMap
      if sameType dbType dataType then Nothing else Just (dbType, dataColumn)
    dbMap =
      columnMap dbColumns

reportMismatchedColumns ::
  Member (Error DbError) r =>
  TableName ->
  NonEmpty (Text, Column) ->
  Sem r ()
reportMismatchedColumns (TableName name) columns =
  throw (DbError.Table [i|mismatched columns in table `#{name}`: #{columnsDescription}|])
  where
    columnsDescription =
      Text.intercalate ";" (toList (columnDescription <$> columns))
    columnDescription (dbType, Column colName dataType _) =
      [i|db: #{colName} :: #{dbType}, app: #{colName} :: #{dataType}|]

updateTable ::
  Members [Embed IO, Error DbError] r =>
  NonEmpty Column ->
  Connection ->
  TableStructure ->
  Sem r ()
updateTable existing connection (TableStructure name target) = do
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
