module Polysemy.Db.StoreQuery where

import Control.Lens (view)
import qualified Data.Map.Strict as Map

import Polysemy.Db.Data.StoreQuery (StoreQuery(..))
import Polysemy.Db.Store (StrictStore(StrictStore))
import qualified Polysemy.Db.Store as StrictStore (records)

interpretStoreQueryStrict ::
  Ord q =>
  (∀ a . [a] -> f a) ->
  Map q [d] ->
  InterpreterFor (StoreQuery q (f d) !! e) r
interpretStoreQueryStrict filter' store =
  interpretResumable \case
    Basic params ->
      pure (filter' (fromMaybe [] (Map.lookup params store)))

single :: [a] -> Maybe a
single [a] = Just a
single _ = Nothing

interpretStoreQueryStrictOne ::
  Ord q =>
  Map q [d] ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretStoreQueryStrictOne =
  interpretStoreQueryStrict single

interpretStoreQueryStrictMulti ::
  Ord q =>
  Map q [d] ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretStoreQueryStrictMulti =
  interpretStoreQueryStrict id

interpretStoreQueryAtomicState ::
  ∀ d q e f r .
  Member (AtomicState (StrictStore d)) r =>
  (∀ a . [a] -> f a) ->
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q (f d) !! e) r
interpretStoreQueryAtomicState filter' match =
  interpretResumable \case
    Basic q ->
      atomicGets @(StrictStore d) (filter' . filter (match q) . view StrictStore.records)

interpretStoreQueryAtomicStateOne ::
  Member (AtomicState (StrictStore d)) r =>
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretStoreQueryAtomicStateOne =
  interpretStoreQueryAtomicState single

interpretStoreQueryAtomicStateMulti ::
  Member (AtomicState (StrictStore d)) r =>
  (q -> d -> Bool) ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretStoreQueryAtomicStateMulti =
  interpretStoreQueryAtomicState id

interpretStoreQueryAtomicTVar ::
  Member (Embed IO) r =>
  TVar (StrictStore d) ->
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
  tvar <- newTVarIO (StrictStore initial)
  interpretStoreQueryAtomicTVar tvar filter' match sem

interpretStoreQueryAtomicOneWith ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  [d] ->
  InterpreterFor (StoreQuery q (Maybe d) !! e) r
interpretStoreQueryAtomicOneWith match initial sem = do
  tvar <- newTVarIO (StrictStore initial)
  interpretStoreQueryAtomicTVar tvar single match sem

interpretStoreQueryAtomicMultiWith ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  [d] ->
  InterpreterFor (StoreQuery q [d] !! e) r
interpretStoreQueryAtomicMultiWith match initial sem = do
  tvar <- newTVarIO (StrictStore initial)
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
