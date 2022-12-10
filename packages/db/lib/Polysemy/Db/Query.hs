module Polysemy.Db.Query where

import Control.Concurrent.STM.TVar (TVar, newTVarIO)
import Control.Lens (view)
import qualified Data.Map.Strict as Map
import qualified Sqel.Data.Uid as Uid

import Polysemy.Db.Effect.Query (Query (..))
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Db.Store (PureStore (PureStore))

interpretQueryPure ::
  Ord q =>
  (∀ a . [a] -> f a) ->
  Map q [d] ->
  InterpreterFor (Query q (f d) !! e) r
interpretQueryPure filter' store =
  interpretResumable \case
    Query params ->
      pure (filter' (fromMaybe [] (Map.lookup params store)))

single :: [a] -> Maybe a
single [a] = Just a
single _ = Nothing

interpretQueryPureOne ::
  Ord q =>
  Map q [d] ->
  InterpreterFor (Query q (Maybe d) !! e) r
interpretQueryPureOne =
  interpretQueryPure single

interpretQueryPureMulti ::
  Ord q =>
  Map q [d] ->
  InterpreterFor (Query q [d] !! e) r
interpretQueryPureMulti =
  interpretQueryPure id

interpretQueryAtomicState ::
  ∀ d q e f r .
  Member (AtomicState (PureStore d)) r =>
  (∀ a . [a] -> f a) ->
  (q -> d -> Bool) ->
  InterpreterFor (Query q (f d) !! e) r
interpretQueryAtomicState filter' match =
  interpretResumable \case
    Query q ->
      atomicGets @(PureStore d) (filter' . filter (match q) . view #records)

interpretQueryAtomicStateOne ::
  Member (AtomicState (PureStore d)) r =>
  (q -> d -> Bool) ->
  InterpreterFor (Query q (Maybe d) !! e) r
interpretQueryAtomicStateOne =
  interpretQueryAtomicState single

interpretQueryAtomicStateMulti ::
  Member (AtomicState (PureStore d)) r =>
  (q -> d -> Bool) ->
  InterpreterFor (Query q [d] !! e) r
interpretQueryAtomicStateMulti =
  interpretQueryAtomicState id

interpretQueryAtomicTVar ::
  Member (Embed IO) r =>
  TVar (PureStore d) ->
  (∀ a . [a] -> f a) ->
  (q -> d -> Bool) ->
  InterpreterFor (Query q (f d) !! e) r
interpretQueryAtomicTVar tvar filter' match sem =
  runAtomicStateTVar tvar . interpretQueryAtomicState filter' match . raiseUnder $ sem

interpretQueryAtomicWith ::
  Member (Embed IO) r =>
  (∀ a . [a] -> f a) ->
  (q -> d -> Bool) ->
  [d] ->
  InterpreterFor (Query q (f d) !! e) r
interpretQueryAtomicWith filter' match initial sem = do
  tvar <- embed (newTVarIO (PureStore initial))
  interpretQueryAtomicTVar tvar filter' match sem

interpretQueryAtomicOneWith ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  [d] ->
  InterpreterFor (Query q (Maybe d) !! e) r
interpretQueryAtomicOneWith match initial sem = do
  tvar <- embed (newTVarIO (PureStore initial))
  interpretQueryAtomicTVar tvar single match sem

interpretQueryAtomicMultiWith ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  [d] ->
  InterpreterFor (Query q [d] !! e) r
interpretQueryAtomicMultiWith match initial sem = do
  tvar <- embed (newTVarIO (PureStore initial))
  interpretQueryAtomicTVar tvar id match sem

interpretQueryAtomicOne ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  InterpreterFor (Query q (Maybe d) !! e) r
interpretQueryAtomicOne match =
  interpretQueryAtomicOneWith match def

interpretQueryAtomicMulti ::
  Member (Embed IO) r =>
  (q -> d -> Bool) ->
  InterpreterFor (Query q [d] !! e) r
interpretQueryAtomicMulti match =
  interpretQueryAtomicMultiWith match def

interpretQueryAny ::
  ∀ q d i e r .
  Member (Store i d !! e) r =>
  (q -> d -> Bool) ->
  InterpreterFor (Query q Bool !! e) r
interpretQueryAny match =
  interpretResumable \case
    Query q ->
      any (match q . Uid.payload) <$> restop Store.fetchAll
