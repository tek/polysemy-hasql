module Polysemy.Hasql.Interpreter.Store where

import Hasql.Connection (Connection)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (QStore, Store)
import Sqel (Crud, DdType, ResultShape, Sqel, Uid)
import qualified Sqel.Crud
import Sqel.Crud (Crud (Crud), crud)
import Sqel.Exts (Check1)

import Polysemy.Hasql.Class.RunStatement (runStatement)
import Polysemy.Hasql.Effect.Database (ConnectionSource)
import Polysemy.Hasql.Effect.DbTable (DbTable, StoreTable)
import Polysemy.Hasql.Transaction (interpretForXa)

handleQStoreDbCrud ::
  ∀ f q d r m a .
  ResultShape d (f d) =>
  Member (DbTable d !! DbError) r =>
  Crud q d ->
  QStore f q d m a ->
  Sem (Stop DbError : r) a
handleQStoreDbCrud Crud {..} = \case
  Store.Insert d ->
    runStatement True d insert
  Store.Upsert d ->
    runStatement True d upsert
  Store.Delete i ->
    runStatement True i delete
  Store.DeleteAll ->
    runStatement True () deleteAll
  Store.Fetch i ->
    runStatement True i fetch
  Store.FetchAll ->
    runStatement True () fetchAll

handleQStoreDb ::
  ∀ f q d ds qs r m a .
  q ~ DdType qs =>
  d ~ DdType ds =>
  Check1 ds qs =>
  ResultShape d (f d) =>
  Member (DbTable d !! DbError) r =>
  Sqel qs ->
  Sqel ds ->
  QStore f q d m a ->
  Sem (Stop DbError : r) a
handleQStoreDb q t =
  handleQStoreDbCrud (crud q t)

interpretQStoreDbCrud ::
  ∀ f q d r .
  ResultShape d (f d) =>
  Member (DbTable d !! DbError) r =>
  Crud q d ->
  InterpreterFor (QStore f q d !! DbError) r
interpretQStoreDbCrud statements =
  interpretResumable (handleQStoreDbCrud statements)

interpretQStoreDb ::
  ∀ f q d ds qs r .
  q ~ DdType qs =>
  d ~ DdType ds =>
  ResultShape d (f d) =>
  Check1 ds qs =>
  Member (DbTable d !! DbError) r =>
  Sqel qs ->
  Sqel ds ->
  InterpreterFor (QStore f q d !! DbError) r
interpretQStoreDb q t =
  interpretResumable (handleQStoreDb q t)

interpretStoreDbCrud ::
  ∀ i d r .
  Member (StoreTable i d !! DbError) r =>
  Crud i (Uid i d) ->
  InterpreterFor (Store i d !! DbError) r
interpretStoreDbCrud =
  interpretQStoreDbCrud

interpretStoreDb ::
  ∀ i d is ds r .
  i ~ DdType is =>
  Uid i d ~ DdType ds =>
  Check1 ds is =>
  Member (StoreTable i d !! DbError) r =>
  Sqel is ->
  Sqel ds ->
  InterpreterFor (Store i d !! DbError) r
interpretStoreDb =
  interpretQStoreDb

interpretQStoreXa ::
  ∀ f q d qs ds r .
  q ~ DdType qs =>
  d ~ DdType ds =>
  Check1 ds qs =>
  ResultShape d (f d) =>
  Members [Scoped ConnectionSource (DbTable d !! DbError), Log, Embed IO] r =>
  Sqel qs ->
  Sqel ds ->
  InterpreterFor (Scoped Connection (QStore f q d !! DbError) !! DbError) r
interpretQStoreXa q t =
  interpretForXa (handleQStoreDb q t)

interpretStoreXa ::
  ∀ i d is ds r .
  i ~ DdType is =>
  Uid i d ~ DdType ds =>
  Check1 ds is =>
  Members [Scoped ConnectionSource (DbTable (Uid i d) !! DbError), Log, Embed IO] r =>
  Sqel is ->
  Sqel ds ->
  InterpreterFor (Scoped Connection (Store i d !! DbError) !! DbError) r
interpretStoreXa =
  interpretQStoreXa @Maybe

interpretQStores ::
  ∀ f q d qs ds r .
  q ~ DdType qs =>
  d ~ DdType ds =>
  Check1 ds qs =>
  ResultShape d (f d) =>
  Members [Scoped ConnectionSource (DbTable d !! DbError), DbTable d !! DbError, Log, Embed IO] r =>
  Sqel qs ->
  Sqel ds ->
  InterpretersFor [QStore f q d !! DbError, Scoped Connection (QStore f q d !! DbError) !! DbError] r
interpretQStores q t =
  interpretQStoreXa q t .
  interpretQStoreDb q t

interpretStores ::
  ∀ i d is ds r .
  i ~ DdType is =>
  Uid i d ~ DdType ds =>
  Check1 ds is =>
  Members [Scoped ConnectionSource (DbTable (Uid i d) !! DbError), StoreTable i d !! DbError, Log, Embed IO] r =>
  Sqel is ->
  Sqel ds ->
  InterpretersFor [Store i d !! DbError, Scoped Connection (Store i d !! DbError) !! DbError] r
interpretStores =
  interpretQStores @Maybe
