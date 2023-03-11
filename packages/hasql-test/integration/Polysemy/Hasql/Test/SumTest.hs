{-# options_ghc -fconstraint-solver-iterations=10 #-}

module Polysemy.Hasql.Test.SumTest where

import Hasql.Statement (Statement)
import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Query as Query
import Polysemy.Db.Effect.Query (Query (Query))
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import Polysemy.Test (UnitTest, (===))
import Prelude hiding (sum)
import Sqel (Sqel, type (:>) ((:>)))
import Sqel (QuerySchema)
import Sqel (TableSchema)
import Sqel (Uid (Uid))
import Sqel (typeAs)
import Sqel.PgType (fullProjection, tableSchema)
import Sqel (prim, primAs, prims)
import Sqel (prod)
import Sqel (checkQuery)
import Sqel.Statement (selectWhere)
import Sqel.Sum (con, conAs, sum)
import Sqel (uid)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.DbTable (interpretTable)
import Polysemy.Hasql.Interpreter.Store (interpretStoreDb)
import Polysemy.Hasql.Test.RunIntegration (integrationTest)

data Pord =
  Pord {
    p1 :: Int,
    p2 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Sumbo =
  Glorpf { g1 :: Int, g2 :: Text }
  |
  Vnarp { v1 :: Int, v2 :: Text }
  |
  Shwank { s1 :: Text, s2 :: Pord }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    sumb :: Sumbo
  }
  deriving stock (Eq, Show, Generic)

data SumboQ =
  GlorpfQ { g1 :: Int }
  |
  ShwankQ { s1 :: Text }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    n :: Text,
    sumb :: SumboQ
  }
  deriving stock (Eq, Show, Generic)

td :: Sqel (Uid Int64 Dat) _
td =
  uid prim (prod (
    prim :>
    typeAs @"sombo" (sum (
      con prims :>
      con prims :>
      con (prim :> prod prims)
    ))
  ))

ts :: TableSchema (Uid Int64 Dat)
ts = tableSchema td

idSchema :: QuerySchema Int64 (Uid Int64 Dat)
idSchema =
  checkQuery (primAs @"id") td

stm :: Statement Q [Uid Int64 Dat]
stm =
  selectWhere (checkQuery qd td) (fullProjection td)
  where
    qd =
      prod (
        primAs @"name" :>
        sum (
          conAs @"Glorpf" prim :>
          conAs @"Shwank" prim
        )
      )

interpretQuery ::
  Member (Database !! DbError) r =>
  InterpreterFor (Query Q [Uid Int64 Dat] !! DbError) r
interpretQuery =
  interpretResumable \case
    Query params ->
      restop (Database.statement params stm)

test_sum :: UnitTest
test_sum =
  integrationTest do
    interpretTable ts $ interpretStoreDb ts idSchema $ interpretQuery do
      restop @DbError @(Query _ _) $ restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "ellow" (Glorpf 5 "crinp")))
        Store.insert (Uid 2 (Dat "ellow" (Glorpf 6 "crinp")))
        Store.insert (Uid 3 (Dat "cheerio" (Shwank "gzerq" (Pord 93 "pord"))))
        r1 <- fmap (view #id) <$> Query.query (Q "ellow" (GlorpfQ 5))
        [1] === r1
        r2 <- fmap (view #id) <$> Query.query (Q "cheerio" (ShwankQ "gzerq"))
        [3] === r2
