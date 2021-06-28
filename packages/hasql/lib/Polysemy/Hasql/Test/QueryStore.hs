module Polysemy.Hasql.Test.QueryStore where

import Polysemy.Db.Data.Rep (Auto, PrimQuery, PrimaryKey, UidRep)
import Polysemy.Db.Data.Uid (Uid)

import qualified Polysemy.Hasql.Data.Query as Query
import Polysemy.Hasql.Data.QueryTable (QueryTable)
import Polysemy.Hasql.Interpreter.QueryStore (QueryStoreStack, interpretQueryStoreDbFullWith)
import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (Schema, UidQuerySchema, UidSchema)
import Polysemy.Hasql.Test.Database (TestStoreDeps, withTestQueryTableGen)

withTestQueryStoreTable ::
  ∀ qrep iqrep rep i d q p r tree a .
  BuildPartialSql p tree =>
  Members TestStoreDeps r =>
  Schema qrep rep q d =>
  Schema iqrep rep i d =>
  (QueryTable q d -> Sem (QueryStoreStack i d q p ++ r) a) ->
  Sem r a
withTestQueryStoreTable prog =
  withTestQueryTableGen @qrep @rep @q @d \ table -> do
    (iParams, iWhere) <- interpretQuery @iqrep @rep (tuple (Query.params @i @d) Query.query)
    interpretQueryStoreDbFullWith table iParams iWhere (prog table)

withTestQueryStoreTableUid ::
  ∀ qrep rep i d q p r tree a .
  BuildPartialSql p tree =>
  Members TestStoreDeps r =>
  UidQuerySchema qrep PrimaryKey rep q i d =>
  UidSchema rep i d =>
  (QueryTable q (Uid i d) -> Sem (QueryStoreStack i (Uid i d) q p ++ r) a) ->
  Sem r a
withTestQueryStoreTableUid =
  withTestQueryStoreTable @qrep @(PrimQuery "id") @(UidRep PrimaryKey rep)

withTestQueryStore ::
  ∀ qrep iqrep rep i d q p r tree .
  BuildPartialSql p tree =>
  Members TestStoreDeps r =>
  Schema qrep rep q d =>
  Schema iqrep rep i d =>
  InterpretersFor (QueryStoreStack i d q p) r
withTestQueryStore prog =
  withTestQueryStoreTable @qrep @iqrep @rep (const prog)

withTestQueryStoreUid ::
  ∀ qrep rep i d q p r tree .
  BuildPartialSql p tree =>
  Members TestStoreDeps r =>
  UidQuerySchema qrep PrimaryKey rep q i d =>
  UidSchema rep i d =>
  InterpretersFor (QueryStoreStack i (Uid i d) q p) r
withTestQueryStoreUid =
  withTestQueryStore @qrep @(PrimQuery "id") @(UidRep PrimaryKey rep)

withTestQueryStoreAuto ::
  ∀ i d q p r tree a .
  BuildPartialSql p tree =>
  Members TestStoreDeps r =>
  Schema Auto Auto q d =>
  Schema Auto Auto i d =>
  Sem (QueryStoreStack i d q p ++ r) a ->
  Sem r a
withTestQueryStoreAuto =
  withTestQueryStore @Auto @Auto @Auto
