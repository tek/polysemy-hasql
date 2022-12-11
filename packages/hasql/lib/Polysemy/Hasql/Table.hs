module Polysemy.Hasql.Table where

import Exon (exon)
import Hasql.Connection (Connection)
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import Hasql.Statement (Statement)
import Polysemy.Db.Data.DbError (DbError)
import qualified Sqel.Data.PgType as PgType
import Sqel.Data.PgType (
  PgColumnName (PgColumnName),
  PgStructure (PgStructure),
  PgTable (PgTable),
  PgTypeRef (PgTypeRef),
  StructureType (StructureComp, StructurePrim),
  )
import Sqel.Data.PgTypeName (PgTableName)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Sql.Type as Sql
import qualified Sqel.Statement as Statement
import Sqel.Statement (plain)

import Polysemy.Hasql.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Polysemy.Hasql.Data.InitDb (ClientTag (ClientTag))
import Polysemy.Hasql.Session (runStatement)

dbColumnsStatement ::
  Sql ->
  Statement Text [(Text, Text, Text)]
dbColumnsStatement sql =
  Statement.prepared sql decoder encoder
  where
    decoder =
      (,,) <$> text' <*> text' <*> text'
    text' =
      Decoders.column (Decoders.nonNullable Decoders.text)
    encoder =
      Encoders.param (Encoders.nonNullable Encoders.text)

dbColumnsFor ::
  Members [Embed IO, Stop DbError] r =>
  Sql ->
  Connection ->
  ClientTag ->
  Sem r (Maybe (NonEmpty ExistingColumn))
dbColumnsFor sql connection (ClientTag tableName) =
  nonEmpty . fmap cons <$> runStatement connection tableName (dbColumnsStatement sql)
  where
    cons (name, dataType, udtName) =
      ExistingColumn (PgColumnName name) dataType udtName

tableColumnsSql :: Sql
tableColumnsSql =
  [exon|select "column_name", "data_type", "udt_name" from information_schema.columns where "table_name" = $1|]

tableColumns ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  ClientTag ->
  Sem r (Maybe (NonEmpty ExistingColumn))
tableColumns =
  dbColumnsFor tableColumnsSql

typeColumnsSql :: Sql
typeColumnsSql =
  [exon|select "attribute_name", "data_type", "attribute_udt_name" from information_schema.attributes where "udt_name" = $1|]

typeColumns ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  PgTypeRef ->
  Sem r (Maybe (NonEmpty ExistingColumn))
typeColumns connection (PgTypeRef name) =
  dbColumnsFor typeColumnsSql connection (ClientTag name)

-- TODO
updateType ::
  NonEmpty ExistingColumn ->
  Sem r ()
updateType _ =
  unit

initComp ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  PgTypeRef ->
  PgStructure ->
  Sem r ()
initComp connection tpe structure = do
  existing <- typeColumns connection tpe
  when (isNothing existing) createType
  where
    createType = do
      initStructure connection structure
      run (plain (Sql.createProdType tpe structure))
    run =
      runStatement connection ()

-- TODO use effects for running statements
initType ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  PgColumnName ->
  StructureType ->
  Sem r ()
initType connection (PgColumnName _) = \case
  StructurePrim _ _ _ ->
    unit
  StructureComp tpe columns ->
    initComp connection tpe columns

initStructure ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  PgStructure ->
  Sem r ()
initStructure connection (PgStructure cols) =
  traverse_ (uncurry (initType connection)) cols

createTable ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  PgTable a ->
  Sem r ()
createTable connection table@PgTable {structure} = do
  initStructure connection structure
  run (plain (Sql.createTable table))
  where
    run =
      runStatement connection ()

dropTable ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  PgTableName ->
  Sem r ()
dropTable connection =
  runStatement connection () . plain . Sql.dropTable

userDefined :: Text
userDefined =
  "USER-DEFINED"
