module Polysemy.Hasql.Test.StoreUpdateTest where

import qualified Data.Aeson as Aeson
import qualified Data.List.NonEmpty as NonEmpty
import Polysemy.Db.Data.Rep (Auto, Prim, PrimQuery, UidRep)
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.PartialField (PartialTree, partially)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UidStore)
import qualified Polysemy.Db.Data.Uid as Uid
import Polysemy.Db.Data.Uid (Uid (Uid))
import qualified Polysemy.Db.Effect.StoreUpdate as StoreUpdate
import Polysemy.Db.Effect.StoreUpdate (StoreUpdate)
import Polysemy.Db.Tree.Partial (field, (+>))
import Polysemy.Db.Tree.Partial.Insert (InsertPaths, PartialUpdate (PartialUpdate), type (@>))
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
    int :: Int,
    double :: Double,
    txt :: Tex
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
  [Uid 1 (Dat 5 73.18 "updated"), keepRecord]

type DatUpdates =
  ["int" @> Int, "double" @> Double, "txt" @> Tex]

update ::
  ∀ tree .
  InsertPaths (Uid Int Dat) DatUpdates tree =>
  PartialTree tree
update =
  partially @(Uid Int Dat) +> field @"int" (5 :: Int) +> field @"double" (73.18 :: Double)

updateWith ::
  ∀ e r .
  Members [StoreUpdate Int (Uid Int Dat) DatUpdates !! e, Stop e] r =>
  PartialUpdate (Uid Int Dat) DatUpdates ->
  Sem r ()
updateWith upd =
  restop (StoreUpdate.partial 1 upd)

prog ::
  ∀ e r .
  Members [UidStore Int Dat !! e, StoreUpdate Int (Uid Int Dat) DatUpdates !! e, Stop e, Error Text] r =>
  Sem r (Maybe (NonEmpty (Uid Int Dat)))
prog = do
  restop (Store.insert updateRecord)
  restop (Store.insert keepRecord)
  restop (StoreUpdate.partial 1 (PartialUpdate update))
  jsonUpdate <- fromEither (mapLeft toText (Aeson.eitherDecode [text|{"_payload":{"txt":"updated"}}|]))
  updateWith jsonUpdate
  restop Store.fetchAll

test_partialDbUpdate :: UnitTest
test_partialDbUpdate =
  integrationTest do
    withTestStoreUid do
      result <- interpretQuery @(PrimQuery "id") @(UidRep Prim Auto) (interpretStoreUpdateDb (prog @DbError))
      assertJust target (NonEmpty.sortWith Uid._id <$> result)
