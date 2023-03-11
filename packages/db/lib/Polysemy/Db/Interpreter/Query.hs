module Polysemy.Db.Interpreter.Query where

import Conc (interpretAtomic)
import qualified Data.Map.Strict as Map
import Lens.Micro.Extras (view)
import qualified Sqel.Data.Uid as Uid
import Sqel (Uid (Uid))

import qualified Polysemy.Db.Data.DbError as DbError
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Effect.Query (Query (..))
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Db.Interpreter.Store (PureStore (PureStore), interpretStoreConc)

class QueryCheckResult f where
  queryCheckResult :: [a] -> Either DbError (f a)

instance QueryCheckResult Maybe where
  queryCheckResult = \case
    [] -> Right Nothing
    [a] -> Right (Just a)
    _ -> Left (DbError.Query "Multiple matches for single-result query")

instance QueryCheckResult [] where
  queryCheckResult = Right

interpretQueryConst ::
  Ord q =>
  QueryCheckResult f =>
  Map q [d] ->
  InterpreterFor (Query q (f d) !! DbError) r
interpretQueryConst store =
  interpretResumable \case
    Query params ->
      stopEither (queryCheckResult (fromMaybe [] (Map.lookup params store)))

interpretQueryAtomicState ::
  ∀ i a d q f r .
  Member (AtomicState (PureStore i a)) r =>
  QueryCheckResult f =>
  (q -> Uid i a -> Maybe d) ->
  InterpreterFor (Query q (f d) !! DbError) r
interpretQueryAtomicState match =
  interpretResumable \case
    Query q ->
      stopEither =<< atomicGets (queryCheckResult . mapMaybe (match q) . Map.elems . view #records)

interpretQueryConc ::
  Ord i =>
  QueryCheckResult f =>
  Member (Embed IO) r =>
  (q -> Uid i a -> Maybe d) ->
  [Uid i a] ->
  InterpreterFor (Query q (f d) !! DbError) r
interpretQueryConc match initial =
  interpretAtomic (PureStore (Map.fromList (initial <&> \ a@(Uid i _) -> (i, a)))) .
  interpretQueryAtomicState match .
  raiseUnder

interpretQueryStoreConc ::
  Ord i =>
  Show i =>
  QueryCheckResult f =>
  Member (Embed IO) r =>
  (q -> Uid i a -> Maybe d) ->
  [Uid i a] ->
  InterpretersFor [Query q (f d) !! DbError, Store i a !! DbError, AtomicState (PureStore i a)] r
interpretQueryStoreConc match initial =
  interpretStoreConc (PureStore (Map.fromList (initial <&> \ a@(Uid i _) -> (i, a)))) .
  interpretQueryAtomicState match

interpretQueryStore ::
  QueryCheckResult f =>
  Member (Store i a !! DbError) r =>
  (q -> Uid i a -> Maybe d) ->
  InterpreterFor (Query q (f d) !! DbError) r
interpretQueryStore match =
  interpretResumable \case
    Query q ->
      stopEither =<< (queryCheckResult . mapMaybe (match q) <$> restop Store.fetchAll)

interpretQueryStoreAny ::
  ∀ q d i e r .
  Member (Store i d !! e) r =>
  (q -> d -> Bool) ->
  InterpreterFor (Query q Bool !! e) r
interpretQueryStoreAny match =
  interpretResumable \case
    Query q ->
      any (match q . Uid.payload) <$> restop Store.fetchAll
