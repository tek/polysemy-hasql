{-# options_ghc -Wno-partial-type-signatures -fconstraint-solver-iterations=10 #-}

module Polysemy.Hasql.Test.UnaryConTest where

import Generics.SOP (NP (Nil, (:*)))
import Hasql.Statement (Statement)
import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import Polysemy.Db.Effect.Query (Query (Query))
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Prelude hiding (sum)
import Sqel.Data.Dd
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.PgType (tableSchema, toFullProjection)
import Sqel.Prim (prim, primAs)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Statement (selectWhere)
import Sqel.Sum (con1, sum)
import Sqel.Uid (uid)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data F2 =
  F2 {
    a2 :: Text,
    b2 :: Int
  }
  deriving stock (Eq, Show, Generic)

data S =
  S1 { f1 :: Text }
  |
  S2 F2
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
  uid prim (prod (
    prim :*
    sum (
      con1 prim :*
      con1 (prod (primAs @"desc" :* prim :* Nil)) :* Nil
    ) :*
    Nil
  ))

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
  selectWhere checkedQ (toFullProjection t1d)

interpretQuery ::
  Member (Database !! DbError) r =>
  InterpreterFor (Query Q [Uid Int64 Dat] !! DbError) r
interpretQuery =
  interpretResumable \case
    Query params ->
      restop (Database.statement params checkedQStm)

test_unaryCon :: UnitTest
test_unaryCon =
  integrationTest do
    interpretTable t1d $ interpretStoreDb t1d (checkQuery (primAs @"id") t1C) $ interpretQuery do
      restop @DbError @(Query _ _) $ restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "ellow" (S1 "crinp")))
        Store.insert (Uid 2 (Dat "cheerio" (S2 (F2 "pord" 93))))
        r <- fmap (view #id) <$> Query.query (Q "ellow" (S1 "crinp"))
        [1] === r
