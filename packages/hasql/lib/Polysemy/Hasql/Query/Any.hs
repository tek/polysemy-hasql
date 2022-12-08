module Polysemy.Hasql.Query.Any where

-- import Polysemy.Db.Data.InitDbError (InitDbError)
-- import Polysemy.Db.Effect.Query (Query)

-- import Polysemy.Hasql.Data.ManagedTable (ManagedTable)
-- import Polysemy.Hasql.Data.Query (Query)
-- import Polysemy.Hasql.ManagedTable (queryTable)
-- import Polysemy.Hasql.Query.Query (interpretStoreQuery)
-- import Polysemy.Hasql.Statement (anyWhere)

-- interpretStoreQueryAny ::
--   ∀ q d e r .
--   Show e =>
--   Members [Query q d, ManagedTable d !! e, Error InitDbError] r =>
--   InterpreterFor (Query q Bool !! e) r
-- interpretStoreQueryAny sem = do
--   table <- queryTable
--   interpretStoreQuery (anyWhere table) sem
