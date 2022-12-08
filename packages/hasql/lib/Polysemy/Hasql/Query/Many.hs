module Polysemy.Hasql.Query.Many where

-- import Polysemy.Db.Data.InitDbError (InitDbError)
-- import Polysemy.Db.Effect.Query (Query(..))

-- import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
-- import Polysemy.Hasql.Data.Query (Query)
-- import Polysemy.Hasql.Data.QueryTable (QueryTable)
-- import Polysemy.Hasql.Query.Query (interpretStoreQueryUsing, interpretStoreQueryWith)
-- import Polysemy.Hasql.Statement (selectWhere)

-- interpretManyWith ::
--   ∀ qOut qIn dIn dOut dResult e r .
--   Member (ManagedTable dIn !! e) r =>
--   QueryTable qIn dIn ->
--   ([dOut] -> dResult) ->
--   (qOut -> qIn) ->
--   (dIn -> dOut) ->
--   InterpreterFor (Query qOut dResult !! e) r
-- interpretManyWith table =
--   interpretStoreQueryWith (selectWhere table)

-- interpretManyAs ::
--   ∀ qOut qIn dIn dOut dResult e r .
--   Show e =>
--   Members [Query qIn dIn, ManagedTable dIn !! e, Error InitDbError] r =>
--   ([dOut] -> dResult) ->
--   (qOut -> qIn) ->
--   (dIn -> dOut) ->
--   InterpreterFor (Query qOut dResult !! e) r
-- interpretManyAs =
--   interpretStoreQueryUsing selectWhere

-- interpretManyAsList ::
--   ∀ qOut qIn dIn dOut e r .
--   Show e =>
--   Members [Query qIn dIn, ManagedTable dIn !! e, Error InitDbError] r =>
--   (qOut -> qIn) ->
--   (dIn -> dOut) ->
--   InterpreterFor (Query qOut [dOut] !! e) r
-- interpretManyAsList =
--   interpretManyAs id

-- interpretMany ::
--   ∀ q d e r .
--   Show e =>
--   Members [Query q d, ManagedTable d !! e, Error InitDbError] r =>
--   InterpreterFor (Query q [d] !! e) r
-- interpretMany =
--   interpretManyAsList id id
