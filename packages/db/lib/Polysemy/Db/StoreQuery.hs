module Polysemy.Db.StoreQuery where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Lens (view)
import qualified Data.Map.Strict as Map

import qualified Sqel.Data.Uid as Uid
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Db.Effect.StoreQuery (StoreQuery (..))
import Polysemy.Db.Store (PureStore (PureStore))
import qualified Polysemy.Db.Store as PureStore (records)

interpretStoreQueryPure ::
  Ord q =>
  (∀ a . [a] -> f a) ->
  Map q [d] ->
  InterpreterFor (StoreQuery q (f d) !! e) r
interpretStoreQueryPure filter' store =
  interpretResumable \case
    Basic params ->
      pure (filter' (fromMaybe [] (Map.lookup params store)))

single :: [a] -> Maybe a
single [a] = Just a
single _ = Nothing

interpretStoreQueryPureOne ::
  Ord q =>
  Map q [d] ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretStoreQueryPureOne =
  interpretStoreQueryPure single

interpretStoreQueryPureMulti ::
  Ord q =>
  Map q [d] ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretStoreQueryPureMulti =
  interpretStoreQueryPure id

interpretStoreQueryAtomicState ::
  ∀ d q e f r .
  Member (AtomicState (PureStore d)) r =>
  (∀ a . [a] -> f a) ->
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q (f d) !! e) r
interpretStoreQueryAtomicState filter' match =
  interpretResumable \case
    Basic q ->
      atomicGets @(PureStore d) (filter' . filter (match q) . view PureStore.records)

interpretStoreQueryAtomicStateOne ::
  Member (AtomicState (PureStore d)) r =>
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretStoreQueryAtomicStateOne =
  interpretStoreQueryAtomicState single

interpretStoreQueryAtomicStateMulti ::
  Member (AtomicState (PureStore d)) r =>
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretStoreQueryAtomicStateMulti =
  interpretStoreQueryAtomicState id

interpretStoreQueryAtomicTVar ::
  Member (Embed IO) r =>
  TVar (PureStore d) ->
  (∀ a . [a] -> f a) ->
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q (f d) !! e) r
interpretStoreQueryAtomicTVar tvar filter' match sem =
  runAtomicStateTVar tvar . interpretStoreQueryAtomicState filter' match . raiseUnder $ sem

interpretStoreQueryAtomicWith ::
  Member (Embed IO) r =>
  (∀ a . [a] -> f a) ->
  (q -> d -> Bool) ->
  [d] ->
  InterpreterFor (StoreQuery q (f d) !! e) r
interpretStoreQueryAtomicWith filter' match initial sem = do
  tvar <- embed (newTVarIO (PureStore initial))
  interpretStoreQueryAtomicTVar tvar filter' match sem

interpretStoreQueryAtomicOneWith ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  [d] ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretStoreQueryAtomicOneWith match initial sem = do
  tvar <- embed (newTVarIO (PureStore initial))
  interpretStoreQueryAtomicTVar tvar single match sem

interpretStoreQueryAtomicMultiWith ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  [d] ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretStoreQueryAtomicMultiWith match initial sem = do
  tvar <- embed (newTVarIO (PureStore initial))
  interpretStoreQueryAtomicTVar tvar id match sem

interpretStoreQueryAtomicOne ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretStoreQueryAtomicOne match =
  interpretStoreQueryAtomicOneWith match def

interpretStoreQueryAtomicMulti ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretStoreQueryAtomicMulti match =
  interpretStoreQueryAtomicMultiWith match def

interpretStoreQueryAny ::
  ∀ q d i e r .
  Member (Store i d !! e) r =>
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q Bool !! e) r
interpretStoreQueryAny match =
  interpretResumable \case
    Basic q ->
      maybe False (any (match q . Uid.payload)) <$> restop @e @(Store i d) (Store.fetchAll @i)
