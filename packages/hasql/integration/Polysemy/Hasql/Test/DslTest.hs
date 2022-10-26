module Polysemy.Hasql.Test.DslTest where

import Generics.SOP (NP (Nil, (:*)))
import Hasql.Encoders (int8, nonNullable, param)
import Polysemy.Db.Data.ColumnPrefix (ColumnPrefix (InitPrefix))
import Polysemy.Db.Data.DbError (DbError)
import Polysemy.Db.Data.Rep (Prim)
import qualified Polysemy.Db.Data.Store as Store
import Polysemy.Db.Data.Uid (Uid (Uid))
import Polysemy.Test (UnitTest)
import Prelude hiding (sum)

import Polysemy.Hasql.Data.QueryTable (QueryTable (QueryTable))
import qualified Polysemy.Hasql.Data.Table as L
import Polysemy.Hasql.Table.Dsl.Api (Table, prim, sum, table, toColumn, uid, prod)
import Polysemy.Hasql.Test.Database (withTestStoreUsing)
import Polysemy.Hasql.Test.Run (integrationTest)
import Polysemy.Hasql.Where (uidWhere)

data Pord =
  Pord {
    p1 :: Int,
    p2 :: Text
  }
  deriving stock (Eq, Show, Generic)

data Sumbo =
  Glorpf { g1 :: Int, g2 :: Text }
  |
  Shwank { s1 :: Text, s2 :: Pord }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    name :: Text,
    sumb :: Sumbo
  }
  deriving stock (Eq, Show, Generic)

testTable :: Table (Uid Int64 Dat)
testTable =
  table (uid prim (
    prim :*
    (sum (
      (prim :* prim :* Nil) :*
      (prim :* (prod (prim :* prim :* Nil)) :* Nil) :* Nil
    )) :* Nil
  ))

legacyTable :: L.Table (Uid Int64 Dat)
legacyTable =
  L.Table (toColumn InitPrefix "dat" (testTable ^. #structure))  (testTable ^. #decoder) (testTable ^. #encoder)

test_dsl :: UnitTest
test_dsl =
  integrationTest do
    withTestStoreUsing (QueryTable legacyTable (param (nonNullable int8)) (uidWhere @'[Prim])) do
      restop @DbError do
        Store.insert (Uid 1 (Dat "ellow" (Glorpf 5 "crinp")))
        Store.insert (Uid 2 (Dat "cheerio" (Shwank "gzerq" (Pord 93 "pord"))))
        dbgs =<< Store.fetchAll
