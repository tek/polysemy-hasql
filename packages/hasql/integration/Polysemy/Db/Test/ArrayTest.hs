module Polysemy.Db.Test.ArrayTest where

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Schema (IdQuery(IdQuery))
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import Polysemy.Db.Data.StoreError (StoreError)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Store (interpretStoreDbFullGen)
import Polysemy.Db.Test (UnitTest, assertRight, evalEither)
import Polysemy.Db.Test.Run (integrationTest)

data ArrayField =
  ArrayField {
    id :: UUID,
    f1 :: [Int]
  }
  deriving (Eq, Show, Generic)

deriveGeneric ''ArrayField

data ArrayFieldRep =
  ArrayFieldRep {
    id :: Prim Auto,
    f1 :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''ArrayFieldRep

prog ::
  Member (Store IdQuery DbError ArrayField) r =>
  Sem r (ArrayField, (Either (StoreError DbError) (Maybe ArrayField)))
prog = do
  let a = ArrayField id' [1, 2, 3]
  Store.upsert a
  b <- Store.fetch (IdQuery id')
  pure (a, b)
  where
    id' = Uid.uuid 555


test_arrayField :: UnitTest
test_arrayField = do
  r <- integrationTest $
    interpretStoreDbFullGen @ArrayField @ArrayFieldRep prog
  (a, b) <- evalEither r
  assertRight (Just a) b
