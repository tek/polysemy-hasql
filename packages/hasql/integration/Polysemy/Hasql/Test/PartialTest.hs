module Polysemy.Hasql.Test.PartialTest where

import Polysemy.Db.Data.Column (Auto, Prim, PrimQuery, UidRep)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UidStore)
import qualified Polysemy.Db.Data.StoreUpdate as StoreUpdate
import Polysemy.Db.Data.StoreUpdate (StoreUpdate)
import Polysemy.Db.Data.Uid (Uid(Uid))
import Polysemy.Db.Tree.Partial (InsertName, field, (+>))
import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Hasql.StoreUpdate (interpretStoreUpdateDb)
import Polysemy.Hasql.Test.Database (withTestStoreUid)
import Polysemy.Hasql.Test.Run (integrationTest)

data Dat =
  Dat {
    int :: Int,
    double :: Double,
    txt :: Text
  }
  deriving (Eq, Show, Generic)

record :: Uid Int Dat
record =
  Uid 1 (Dat 9 12.7 "text")

target :: Uid Int Dat
target =
  Uid 1 (Dat 5 73.18 "text")

prog ::
  ∀ e tree r .
  InsertName "int" Int tree =>
  InsertName "double" Double tree =>
  Members [UidStore Int Dat !! e, StoreUpdate Int (Uid Int Dat) tree !! e, Stop e] r =>
  Sem r (Maybe (NonEmpty (Uid Int Dat)))
prog = do
  restop (Store.insert record)
  restop (StoreUpdate.update 1 \ ptree -> ptree +> field @"int" (5 :: Int) +> field @"double" (73.18 :: Double))
  restop (Store.fetchAll)

test_partialDbUpdate :: UnitTest
test_partialDbUpdate =
  integrationTest do
    withTestStoreUid @Int @Dat do
      assertJust (pure target) =<< interpretQuery @(PrimQuery "id") @(UidRep Prim Auto) (interpretStoreUpdateDb (prog @DbError))
