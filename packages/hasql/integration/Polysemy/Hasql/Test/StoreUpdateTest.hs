module Polysemy.Hasql.Test.StoreUpdateTest where

import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Partial (partial)
import Polysemy.Db.Data.PartialField (PartialTree, partially)
import Polysemy.Db.Data.PartialUpdate (PartialUpdate (PartialUpdate))
import Polysemy.Db.Data.Rep (Prim, PrimQuery, UidRep)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (Store)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid (Uid))
import qualified Polysemy.Db.Effect.StoreUpdate as StoreUpdate
import Polysemy.Db.Effect.StoreUpdate (StoreUpdate)
import Polysemy.Db.Tree.Partial (field, (++>), (+>))
import Polysemy.Db.Tree.Partial.Insert (InsertPaths, type (@>))
import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Hasql.Interpreter.StoreUpdate (interpretStoreUpdateDb)
import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Hasql.Test.Database (withTestStoreUid)
import Polysemy.Hasql.Test.Run (integrationTest)

newtype Tex =
  Tex { unTex :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

defaultJson ''Tex

data Dat =
  Dat {
    intField :: Int,
    double :: Double,
    txt :: Tex,
    boo :: Bool
  }
  deriving (Eq, Show, Generic)

data DatRep =
  DatRep {
    intField :: Prim,
    double :: Prim,
    txt :: Prim,
    boo :: Prim
  }
  deriving (Eq, Show, Generic)

updateRecord :: Uid Int Dat
updateRecord =
  Uid 1 (Dat 9 12.7 "text" True)

keepRecord :: Uid Int Dat
keepRecord =
  Uid 2 (Dat 1 1 "one" True)

type DatUpdates =
  ["intField" @> Int, "double" @> Double, "txt" @> Tex, "boo" @> Bool]

update ::
  ∀ tree .
  InsertPaths Dat DatUpdates tree =>
  PartialTree tree
update =
  partially @Dat ++> field @"intField" (5 :: Int) ++> field @"double" (73.18 :: Double)

updateWith ::
  ∀ e r .
  Members [StoreUpdate Int Dat DatUpdates !! e, Stop e] r =>
  PartialUpdate Dat DatUpdates ->
  Sem r ()
updateWith upd =
  restop (void (StoreUpdate.create 1 upd))

prog ::
  ∀ e r .
  Members [Store Int Dat !! e, StoreUpdate Int Dat DatUpdates !! e, Stop e, Error Text] r =>
  Sem r (Maybe (NonEmpty (Uid Int Dat)))
prog = do
  restop (Store.insert updateRecord)
  restop (Store.insert keepRecord)
  updateWith (PartialUpdate update)
  restop (Store.update 1 (partial @Dat +> field @"intField" (99 :: Int) +> field @"boo" False))
  jsonUpdate <- fromEither (mapLeft toText (Aeson.eitherDecode [text|{"txt":"updated"}|]))
  updateWith jsonUpdate
  restop Store.fetchAll

target :: NonEmpty (Uid Int Dat)
target =
  [Uid 1 (Dat 99 73.18 "updated" False), keepRecord]

test_partialDbUpdate :: UnitTest
test_partialDbUpdate =
  integrationTest do
    withTestStoreUid do
      result <- interpretQuery @(PrimQuery "id") @(UidRep Prim DatRep) (interpretStoreUpdateDb (prog @DbError))
      assertJust target (NonEmpty.sortWith Uid._id <$> result)
