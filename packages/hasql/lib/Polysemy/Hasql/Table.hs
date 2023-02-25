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
  PgComposite (PgComposite),
  PgStructure (PgStructure),
  PgTable (PgTable),
  StructureType (StructureComp, StructurePrim),
  structureToColumns,
  )
import Sqel.Data.PgTypeName (PgCompName, PgTableName, pattern PgTypeName)
import Sqel.Data.Sql (Sql)
import qualified Sqel.Sql.Type as Sql
import qualified Sqel.Statement as Statement
import Sqel.Statement (plain, typeColumnsSql)

import Polysemy.Hasql.Data.ExistingColumn (ExistingColumn (ExistingColumn))
import Polysemy.Hasql.Data.InitDb (ClientTag (ClientTag))
import Polysemy.Hasql.Session (runStatement)

dbColumnsFor ::
  Members [Embed IO, Stop DbError] r =>
  Sql ->
  Connection ->
  ClientTag ->
  Sem r (Maybe (NonEmpty ExistingColumn))
dbColumnsFor sql connection (ClientTag tableName) =
  nonEmpty . fmap cons <$> runStatement connection tableName (Statement.dbColumns sql)
  where
    cons (name, dataType, udtName) =
      ExistingColumn (PgColumnName name) dataType udtName

typeColumns ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  PgCompName ->
  Sem r (Maybe (NonEmpty ExistingColumn))
typeColumns connection (PgTypeName name) =
  dbColumnsFor typeColumnsSql connection (ClientTag name)

initComp ::
  Members [Embed IO, Stop DbError] r =>
  Connection ->
  PgCompName ->
  PgStructure ->
  Sem r ()
initComp connection tpe structure = do
  existing <- typeColumns connection tpe
  when (isNothing existing) createType
  where
    createType = do
      initStructure connection structure
      run (plain (Sql.createProdType (PgComposite tpe (structureToColumns structure))))
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
  StructureComp tpe columns _ _ ->
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
