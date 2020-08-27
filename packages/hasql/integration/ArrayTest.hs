module ArrayTest where

-- import Polysemy.Db.Data.DbError (DbError)
-- import qualified Polysemy.Db.Data.Id as Id
-- import Polysemy.Db.Data.Id (Id)
-- import Polysemy.Db.Data.Schema (IdQuery(IdQuery))
-- import qualified Polysemy.Db.Data.Store as Store
-- import Polysemy.Db.Data.Store (Store)
-- import Polysemy.Db.Data.StoreError (StoreError)
-- import Polysemy.Db.Id (interpretIdUuidIO)
-- import Polysemy.Db.Store (interpretStoreDbSingle)
-- import Polysemy.Db.Test (UnitTest, assertRight, evalEither)

-- data ArrayField =
--   ArrayField {
--     id :: UUID,
--     f1 :: [Int]
--   }
--   deriving (Eq, Show, Generic)

-- deriveGeneric ''ArrayField

-- prog ::
--   Members [Id UUID, Store IdQuery DbError ArrayField] r =>
--   Sem r (ArrayField, (Either (StoreError DbError) (Maybe ArrayField)))
-- prog = do
--   id' <- Id.generate
--   let a = ArrayField id' [1, 2, 3]
--   Store.upsert a
--   b <- Store.fetch (IdQuery id')
--   pure (a, b)

-- test_arrayField :: UnitTest
-- test_arrayField = do
--   r <-
--     lift $
--     runFinal $
--     embedToFinal $
--     runError $
--     interpretIdUuidIO $
--     interpretStoreDbSingle "localhost" $
--     prog
--   (a, b) <- evalEither r
--   assertRight (Just a) b
