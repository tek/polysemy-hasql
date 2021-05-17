module Polysemy.Hasql.Test.PartialTest where

import qualified Data.List.NonEmpty as NonEmpty
import Polysemy.Db.Data.Column (Auto, Prim, PrimQuery, UidRep)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UidStore)
import qualified Polysemy.Db.Data.StoreUpdate as StoreUpdate
import Polysemy.Db.Data.StoreUpdate (StoreUpdate)
import qualified Polysemy.Db.Data.Uid as Uid
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

updateRecord :: Uid Int Dat
updateRecord =
  Uid 1 (Dat 9 12.7 "text")

keepRecord :: Uid Int Dat
keepRecord =
  Uid 2 (Dat 1 1 "one")

target :: NonEmpty (Uid Int Dat)
target =
  [Uid 1 (Dat 5 73.18 "text"), keepRecord]

prog ::
  ∀ e tree r .
  InsertName "int" Int tree =>
  InsertName "double" Double tree =>
  Members [UidStore Int Dat !! e, StoreUpdate Int (Uid Int Dat) tree !! e, Stop e] r =>
  Sem r (Maybe (NonEmpty (Uid Int Dat)))
prog = do
  restop (Store.insert updateRecord)
  restop (Store.insert keepRecord)
  restop (StoreUpdate.update 1 \ ptree -> ptree +> field @"int" (5 :: Int) +> field @"double" (73.18 :: Double))
  restop (Store.fetchAll)

test_partialDbUpdate :: UnitTest
test_partialDbUpdate =
  integrationTest do
    withTestStoreUid @Int @Dat do
      result <- interpretQuery @(PrimQuery "id") @(UidRep Prim Auto) (interpretStoreUpdateDb (prog @DbError))
      assertJust target (NonEmpty.sortWith Uid._id <$> result)
