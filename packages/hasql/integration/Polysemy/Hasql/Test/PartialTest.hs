module Polysemy.Hasql.Test.PartialTest where

import Polysemy.Db.Data.Column (Auto, Prim, PrimQuery, UidRep)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Store (UidStore)
import qualified Polysemy.Db.Data.StoreUpdate as StoreUpdate
import Polysemy.Db.Data.StoreUpdate (StoreUpdate)
import Polysemy.Db.Data.Uid (Uid(Uid))
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree.Partial (InsertName, Partial, PartialTree, field, partial, (+>))
import Polysemy.Test (UnitTest, assertJust)

import Polysemy.Hasql.Query (interpretQuery)
import Polysemy.Hasql.StoreUpdate (interpretStoreUpdateDb)
import Polysemy.Hasql.Test.Database (withTestStoreUid)
import Polysemy.Hasql.Test.Run (integrationTest)

data Dat =
  Dat { int :: Int }
  deriving (Eq, Show, Generic)

class FooTree tree where
  fooTree :: PartialTree tree -> Text

instance FooTree ('Kind.Tree name eff node) where
  fooTree _ = "hello"

class UsePTree (tree :: Kind.Tree) where
  usePTree :: PartialTree tree -> Text

instance FooTree tree => UsePTree tree where
  usePTree tree =
    fooTree tree

data PStore (d :: Type) (tree :: Kind.Tree) :: Effect where
  Update :: (PartialTree tree -> PartialTree tree) -> PStore d tree m ()

makeSem ''PStore

interpretPStore ::
  ∀ d tree r .
  Partial d tree =>
  UsePTree tree =>
  InterpreterFor (PStore d tree) r
interpretPStore =
  interpret \case
    Update tree ->
      dbgs (usePTree (tree ptree))
  where
    ptree =
      partial @d

prog0 ::
  ∀ tree r .
  Member (PStore Dat tree) r =>
  InsertName "int" Int tree =>
  Sem r ()
prog0 = do
  update \ ptree -> ptree +> field @"int" (5 :: Int)

record :: Uid Int Dat
record =
  Uid 1 (Dat 9)

target :: Uid Int Dat
target =
  Uid 1 (Dat 5)

prog ::
  ∀ e tree r .
  InsertName "int" Int tree =>
  Members [UidStore Int Dat !! e, StoreUpdate Int (Uid Int Dat) tree !! e, Stop e] r =>
  Sem r (Maybe (NonEmpty (Uid Int Dat)))
prog = do
  restop (Store.insert record)
  resuming (\ _ -> unit) (StoreUpdate.update 1 \ ptree -> ptree +> field @"int" (5 :: Int))
  restop (Store.fetchAll)

test_partialDbUpdate :: UnitTest
test_partialDbUpdate =
  integrationTest do
    withTestStoreUid @Int @Dat do
      assertJust (pure target) =<< interpretQuery @(PrimQuery "id") @(UidRep Prim Auto) (interpretStoreUpdateDb (prog @DbError))
