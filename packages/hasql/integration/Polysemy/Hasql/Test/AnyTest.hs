module Polysemy.Hasql.Test.AnyTest where

-- import Polysemy.Db.Data.DbError (DbError)
-- import qualified Polysemy.Db.Effect.Store as Store
-- import Polysemy.Db.Effect.Store (Store)
-- import qualified Polysemy.Db.Effect.StoreQuery as StoreQuery
-- import Polysemy.Db.Effect.StoreQuery (StoreQuery)
-- import Sqel.Data.Uid (Uid(Uid))
-- import Polysemy.Test (Hedgehog, UnitTest, assert)

-- import Polysemy.Hasql.Test.Run (integrationTest)

-- data Dat =
--   Dat {
--     name :: Text
--   }
--   deriving stock (Eq, Show, Generic)

-- specimen :: Uid Int Dat
-- specimen =
--   Uid 1 (Dat "name")

-- prog ::
--   Members [Store Int Dat, StoreQuery Dat Bool, Hedgehog IO] r =>
--   Sem r ()
-- prog = do
--   Store.insert (Uid 1 (Dat "first"))
--   assert =<< StoreQuery.basic (Dat "first")
--   assert . not =<< StoreQuery.basic (Dat "second")

-- test_any :: UnitTest
-- test_any =
--   integrationTest do
--       restop @DbError @(StoreQuery Dat Bool) $ restop @DbError @(Store Int Dat) prog
