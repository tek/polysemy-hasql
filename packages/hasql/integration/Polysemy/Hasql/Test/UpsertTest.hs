module Polysemy.Hasql.Test.UpsertTest where

-- import Polysemy.Db.Data.DbError (DbError)
-- import qualified Polysemy.Db.Effect.Store as Store
-- import Polysemy.Db.Effect.Store (Store)
-- import Sqel.Data.Uid (Uid(Uid))
-- import Polysemy.Db.Store (interpretStoreAtomic)
-- import Polysemy.Test (UnitTest, assertJust, evalEither, runTestAuto)

-- import Polysemy.Hasql.Test.Database (withTestStoreUid)
-- import Polysemy.Hasql.Test.Run (integrationTest)

-- data Dat =
--   Dat {
--     name :: Text
--   }
--   deriving stock (Eq, Show, Generic)

-- specimen :: Uid Int Dat
-- specimen =
--   Uid 1 (Dat "second")

-- prog ::
--   Member (Store Int Dat) r =>
--   Sem r (Maybe (NonEmpty (Uid Int Dat)))
-- prog = do
--   Store.insert (Uid 1 (Dat "first"))
--   Store.upsert specimen
--   Store.fetchAll

-- test_upsert :: UnitTest
-- test_upsert = do
--   integrationTest do
--     withTestStoreUid do
--       assertJust [specimen] =<< restop @DbError prog

-- test_upsert_strict :: UnitTest
-- test_upsert_strict =
--   runTestAuto do
--     r <- runStop @() $ interpretStoreAtomic @Int @Dat def do
--       restop @() prog
--     assertJust [specimen] =<< evalEither r
