module Polysemy.Hasql.Query.One where

-- import Polysemy.Db.Data.InitDbError (InitDbError)
-- import Polysemy.Db.Effect.Query (Query(..))

-- import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
-- import Polysemy.Hasql.Data.Query (Query)
-- import Polysemy.Hasql.Data.QueryTable (QueryTable)
-- import Polysemy.Hasql.Query.Query (interpretStoreQueryUsing, interpretStoreQueryWith)
-- import Polysemy.Hasql.Statement (selectWhere)

-- interpretOneWith ::
--   ∀ qOut qIn dIn dOut e r .
--   Member (ManagedTable dIn !! e) r =>
--   QueryTable qIn dIn ->
--   (qOut -> qIn) ->
--   (dIn -> dOut) ->
--   InterpreterFor (Query qOut (Maybe dOut) !! e) r
-- interpretOneWith table =
--   interpretStoreQueryWith (selectWhere table) id

-- interpretOneAs ::
--   ∀ qOut qIn dIn dOut e r .
--   Show e =>
--   Members [Query qIn dIn, ManagedTable dIn !! e, Error InitDbError] r =>
--   (qOut -> qIn) ->
--   (dIn -> dOut) ->
--   InterpreterFor (Query qOut (Maybe dOut) !! e) r
-- interpretOneAs =
--   interpretStoreQueryUsing selectWhere id

-- interpretOne ::
--   ∀ q d e r .
--   Show e =>
--   Members [Query q d, ManagedTable d !! e, Error InitDbError] r =>
--   InterpreterFor (Query q (Maybe d) !! e) r
-- interpretOne =
--   interpretOneAs id id
