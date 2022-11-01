{-# options_ghc -Wno-partial-type-signatures -fconstraint-solver-iterations=10 #-}

module Polysemy.Hasql.Test.Dsl.SumTest where

import Hasql.Statement (Statement)
import Lens.Micro.Extras (view)
import Polysemy.Db.Data.DbError (DbError)
import qualified Polysemy.Db.Effect.Store as Store
import Polysemy.Db.Effect.Store (Store)
import qualified Polysemy.Db.Effect.StoreQuery as StoreQuery
import Polysemy.Db.Effect.StoreQuery (StoreQuery (Basic))
import Polysemy.Test (UnitTest, (===))
import Prelude hiding (sum)
import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid (Uid))
import Sqel.Names (typeAs)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, primAs, prims)
import Sqel.Product (prod, uid)
import Sqel.Query (checkQuery)
import Sqel.Statement (qStatement)
import Sqel.Sum (con, conAs, sum)

import qualified Polysemy.Hasql.Effect.Database as Database
import Polysemy.Hasql.Effect.Database (Database)
import Polysemy.Hasql.Interpreter.Store (interpretManagedTable, interpretStoreDb)
import Polysemy.Hasql.Test.Run (integrationTest)

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
    name :: Text,
    sumb :: SumboQ
  }
  deriving stock (Eq, Show, Generic)

td :: Dd ('DdK _ _ (Uid Int64 Dat) _)
td =
  uid prim (
    prim :>
    sum (
      con prims :>
      con prims :>
      con (prim :> prod prims)
    )
  )

ts :: TableSchema (Uid Int64 Dat)
ts = tableSchema td

idSchema :: QuerySchema Int64 (Uid Int64 Dat)
idSchema =
  checkQuery (primAs @"id") td

interpretQ ::
  Member (Database !! DbError) r =>
  InterpreterFor (StoreQuery Q [Uid Int64 Dat] !! DbError) r
interpretQ =
  interpretResumable \case
    Basic params ->
      restop (Database.statement params stm)
  where
    stm :: Statement Q [Uid Int64 Dat]
    stm =
      qStatement (checkQuery qd td) (tableSchema td)
    qd =
      prod (
        primAs @"name" :>
        typeAs @"Sumbo" (sum (
          conAs @"Glorpf" prim :>
          conAs @"Shwank" prim
        ))
      )

test_dslSum :: UnitTest
test_dslSum =
  integrationTest do
    interpretManagedTable ts $ interpretStoreDb ts idSchema $ interpretQ do
      restop @DbError @(StoreQuery _ _) $ restop @DbError @(Store _ _) do
        Store.insert (Uid 1 (Dat "ellow" (Glorpf 5 "crinp")))
        Store.insert (Uid 2 (Dat "ellow" (Glorpf 6 "crinp")))
        Store.insert (Uid 3 (Dat "cheerio" (Shwank "gzerq" (Pord 93 "pord"))))
        r1 <- fmap (view #id) <$> StoreQuery.basic (Q "ellow" (GlorpfQ 5))
        [1] === r1
        r2 <- fmap (view #id) <$> StoreQuery.basic (Q "ellow" (ShwankQ "gzerq"))
        [3] === r2
