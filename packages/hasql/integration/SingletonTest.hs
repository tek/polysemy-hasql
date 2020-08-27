module SingletonTest where

-- import Hasql.Session (QueryError)
-- import Polysemy.Resource (resourceToIOFinal)

-- import Polysemy.Db.Data.DbError (DbError)
-- import qualified Polysemy.Db.Data.Id as Id
-- import Polysemy.Db.Data.Id (Id)
-- import qualified Polysemy.Db.Data.Store as Store
-- import Polysemy.Db.Data.Store (Store)
-- import Polysemy.Db.Data.StoreError (StoreError)
-- import Polysemy.Db.Data.Table (Table(Table))
-- import Polysemy.Db.Database (interpretDatabase)
-- import Polysemy.Db.DbConnection (interpretDbConnection)
-- import Polysemy.Db.Id (interpretIdUuidIO)
-- import Polysemy.Db.Random (runRandomIO)
-- import Polysemy.Db.Schema.Generic (interpretSchemaSingleton)
-- import Polysemy.Db.Store (interpretStoreDb)
-- import Polysemy.Db.Table.Table (genTable)
-- import Polysemy.Db.Test (UnitTest, assertRight, evalEither)
-- import Polysemy.Db.Test.Database (withTestPlainTable)

-- data Dat =
--   Dat {
--      id :: UUID,
--      content :: Text
--   }
--   deriving (Eq, Show)

-- deriveGeneric ''Dat

-- prog ::
--   Members [Id UUID, Store () DbError Dat] r =>
--   Sem r (Dat, (Either (StoreError DbError) (Maybe (NonEmpty Dat))))
-- prog = do
--   id' <- Id.generate
--   let a = Dat id' "foo"
--   Store.upsert a
--   b <- Store.fetchAll
--   pure (a, b)

-- test_singletonDb :: UnitTest
-- test_singletonDb = do
--   r <-
--     lift $
--     runFinal $
--     embedToFinal $
--     resourceToIOFinal $
--     runError @QueryError $
--     runError @DbError $
--     runRandomIO $
--     interpretIdUuidIO $
--     interpretDbConnection "localhost" $
--     withTestPlainTable (genTable @Dat) $ \table@(Table structure _ _) -> do
--       interpretDatabase structure $
--         interpretSchemaSingleton table $
--         interpretStoreDb $
--         prog
--   (a, b) <- evalEither =<< evalEither r
--   assertRight (Just (pure a)) b
