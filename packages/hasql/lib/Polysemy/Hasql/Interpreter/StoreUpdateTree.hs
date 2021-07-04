module Polysemy.Hasql.Interpreter.StoreUpdateTree where

import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.InitDbError (InitDbError)
import qualified Polysemy.Db.Effect.StoreUpdateTree as StoreUpdateTree
import Polysemy.Db.Effect.StoreUpdateTree (StoreUpdateTree)

import qualified Polysemy.Hasql.Data.ManagedTable as ManagedTable
import Polysemy.Hasql.Data.ManagedTable (ManagedTableUid)
import Polysemy.Hasql.Data.Query (UidQuery)
import Polysemy.Hasql.Data.QueryTable (UidQueryTable)
import Polysemy.Hasql.ManagedTable (queryTable)
import Polysemy.Hasql.Query (interpretQueryUid)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Store (StoreDeps, StoreStack, interpretStoreDbFullGenAs)
import Polysemy.Hasql.Table.Query.Update (BuildPartialSql)
import Polysemy.Hasql.Table.Schema (UidQuerySchema)

interpretStoreUpdateTreeDbWith ::
  ∀ i d e tree r .
  BuildPartialSql d tree =>
  Member (ManagedTableUid i d !! e) r =>
  UidQueryTable i d ->
  InterpreterFor (StoreUpdateTree i d tree !! e) r
interpretStoreUpdateTreeDbWith table =
  interpretResumable \case
    StoreUpdateTree.Partial i t ->
      restop (ManagedTable.runStatement () (Statement.update table i t))
{-# inline interpretStoreUpdateTreeDbWith #-}

interpretStoreUpdateTreeDb ::
  ∀ i d e tree r .
  Show e =>
  BuildPartialSql d tree =>
  Members [UidQuery i d, ManagedTableUid i d !! e, Error InitDbError] r =>
  InterpreterFor (StoreUpdateTree i d tree !! e) r
interpretStoreUpdateTreeDb sem = do
  table <- queryTable
  interpretStoreUpdateTreeDbWith @_ @_ @_ @tree table sem
{-# inline interpretStoreUpdateTreeDb #-}

interpretStoreUpdateTreeDbFull ::
  ∀ qrep irep rep i d tree t dt r .
  UidQuerySchema qrep irep rep i i d =>
  BuildPartialSql d tree =>
  Members (Error InitDbError : StoreDeps t dt) r =>
  InterpretersFor (StoreUpdateTree i d tree !! DbError : StoreStack i d) r
interpretStoreUpdateTreeDbFull =
  interpretStoreDbFullGenAs @qrep @irep @rep @i @d .
  interpretQueryUid @qrep @irep @rep .
  interpretStoreUpdateTreeDb @i @d .
  raiseUnder
{-# inline interpretStoreUpdateTreeDbFull #-}
