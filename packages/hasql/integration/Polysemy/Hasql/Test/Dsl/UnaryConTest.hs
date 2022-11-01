{-# options_ghc -Wno-partial-type-signatures -fconstraint-solver-iterations=10 #-}

module Polysemy.Hasql.Test.Dsl.UnaryConTest where

import Generics.SOP (NP (Nil, (:*)))
import Hasql.Statement (Statement)
import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import qualified Polysemy.Db.Effect.StoreQuery as StoreQuery
import Polysemy.Db.Effect.StoreQuery (StoreQuery (Basic))
import Polysemy.Test (UnitTest, (===))
import Prelude hiding (sum)
import Sqel.Data.Dd
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primAs)
import Sqel.Product (prod, uid)
import Sqel.Query (checkQuery)
import Sqel.Statement (qStatement)
import Sqel.Sum (con1, sum)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.Store (interpretManagedTable, interpretStoreDb)
import Polysemy.Hasql.Test.Run (integrationTest)

data F2 =
  F2 {
    a2 :: Text,
    b2 :: Int
  }
  deriving stock (Eq, Show, Generic)

data S =
  S1 { f1 :: Text }
  |
  S2 { f2 :: F2 }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    s :: S
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    name :: Text,
    s :: S
  }
  deriving stock (Eq, Show, Generic)

t1C :: Dd ('DdK _ _ (Uid Int64 Dat) _)
t1C =
  uid prim (
    prim :*
    sum (
      con1 prim :*
      con1 (prod (primAs @"desc" :* prim :* Nil)) :* Nil
    ) :*
    Nil
  )

q :: Dd ('DdK _ _ Q _)
q =
  prod (
    primAs @"name" :*
    sum (
      con1 prim :*
      con1 (prod (primAs @"desc" :* prim :* Nil)) :* Nil
    ) :*
    Nil
  )

t1d :: TableSchema (Uid Int64 Dat)
t1d =
  tableSchema t1C

checkedQ :: QuerySchema Q (Uid Int64 Dat)
checkedQ =
  checkQuery q t1C

checkedQStm :: Statement Q [Uid Int64 Dat]
checkedQStm =
  qStatement checkedQ t1d

interpretQ ::
  Member (Database !! DbError) r =>
  InterpreterFor (StoreQuery Q [Uid Int64 Dat] !! DbError) r
interpretQ =
  interpretResumable \case
    Basic params ->
      restop (Database.statement params checkedQStm)

test_dslUnaryCon :: UnitTest
test_dslUnaryCon =
  integrationTest do
    interpretManagedTable t1d $ interpretStoreDb t1d (checkQuery (primAs @"id") t1C) $ interpretQ do
      restop @DbError @(StoreQuery _ _) $ restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "ellow" (S1 "crinp")))
        Store.insert (Uid 2 (Dat "cheerio" (S2 (F2 "pord" 93))))
        r <- fmap (view #id) <$> StoreQuery.basic (Q "ellow" (S1 "crinp"))
        [1] === r
