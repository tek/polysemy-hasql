module Polysemy.Db.Interpreter.Store where

import Conc (interpretAtomic)
import qualified Data.Map.Strict as Map
import Exon (exon)
import Sqel.Data.Uid (Uid (Uid))

import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Effect.Store (QStore (..), Store)

newtype PureStore i a =
  PureStore { records :: Map i (Uid i a) }
  deriving stock (Eq, Show, Generic)
  deriving newtype (Default, Semigroup, Monoid)

interpretStoreAtomicState ::
  ∀ i a r .
  Ord i =>
  Show i =>
  Member (AtomicState (PureStore i a)) r =>
  InterpreterFor (Store i a !! DbError) r
interpretStoreAtomicState =
  interpretResumable \case
    Insert a@(Uid i _) ->
      stopEither =<< atomicState' \ (PureStore records) ->
        let
          update = \case
            Just x -> (Left (DbError.Query [exon|'#{show i}' is already in the store|]), Just x)
            Nothing -> (Right (), Just a)
        in first PureStore (swap (Map.alterF update i records))
    Upsert a@(Uid i _) ->
      atomicModify' (#records %~ Map.insert i a)
    Delete i ->
      atomicState' \ (PureStore as) -> (PureStore (Map.delete i as), Map.lookup i as)
    DeleteAll ->
      atomicState' \ (PureStore as) -> (mempty, Map.elems as)
    Fetch i ->
      atomicGets \ (PureStore as) -> Map.lookup i as
    FetchAll ->
      atomicGets \ (PureStore as) -> Map.elems as

interpretStoreConc ::
  ∀ i a r .
  Ord i =>
  Show i =>
  Member (Embed IO) r =>
  PureStore i a ->
  InterpreterFor (Store i a !! DbError) r
interpretStoreConc initial =
  interpretAtomic initial .
  interpretStoreAtomicState .
  raiseUnder

interpretStoreState ::
  ∀ i a r .
  Ord i =>
  Show i =>
  Member (State (PureStore i a)) r =>
  InterpreterFor (Store i a !! DbError) r
interpretStoreState =
  atomicStateToState .
  interpretStoreAtomicState .
  raiseUnder

interpretStoreLocal ::
  ∀ i a r .
  Ord i =>
  Show i =>
  PureStore i a ->
  InterpreterFor (Store i a !! DbError) r
interpretStoreLocal initial =
  evalState initial .
  interpretStoreState .
  raiseUnder

interpretStoreNull ::
  InterpreterFor (Store i a !! e) r
interpretStoreNull =
  interpretResumable \case
    Insert _ ->
      unit
    Upsert _ ->
      unit
    Delete _ ->
      pure Nothing
    DeleteAll ->
      pure []
    Fetch _ ->
      pure Nothing
    FetchAll ->
      pure []
