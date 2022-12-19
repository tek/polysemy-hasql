{-# options_ghc -Wno-partial-type-signatures -fconstraint-solver-iterations=100 #-}

module Sqel.Test.StatementTest where

import Hedgehog (TestT, (===))
import Prelude hiding (sum)
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Data.Dd (Dd, DdK (DdK), type (:>) ((:>)))
import Sqel.Data.Order (Order (Desc))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Select (Select (Select))
import Sqel.Data.Sql (Sql, sql, toSql)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)
import Sqel.Merge (merge)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim, prims)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Query.Combinators (order)
import qualified Sqel.Sql.Select as Sql
import Sqel.Sum (con, sum)
import Sqel.Uid (uid)

data Prod =
  Prod {
    num :: Int64,
    name :: Text
  }
  deriving stock (Eq, Show, Generic)

data Dat =
  Dat {
    num :: Int
  }
  deriving stock (Eq, Show, Generic)

data Q =
  Q {
    num :: ()
  }
  deriving stock (Eq, Show, Generic)

target_order :: Sql
target_order =
  [sql|select "num" from "dat" order by "num" desc|]

qs :: QuerySchema Q Dat
qs =
  checkQuery (prod (order Desc)) (prod prim)

ts :: TableSchema Dat
ts =
  tableSchema (prod prim)

test_statement_order :: TestT IO ()
test_statement_order =
  target_order === Sql.selectWhere qs ts

target_mergeSum :: Sql
target_mergeSum =
  [sql|select "id", "ph_sum_index__merge_sum", ("merge_sum1").num1, ("merge_sum1").name1, ("merge_sum2").num2,
       ("merge_sum2").name2 from "merge_sum"|]

data MergeSum =
  MergeSum1 { num1 :: Int, name1 :: Text }
  |
  MergeSum2 { num2 :: Int, name2 :: Text }
  deriving stock (Eq, Show, Generic)

dd_uid_merge_sum_manual :: Dd ('DdK _ _ (Uid Int64 MergeSum) _)
dd_uid_merge_sum_manual =
  prod (prim :> merge (sum (con prims :> con prims)))

dd_uid_merge_sum :: Dd ('DdK _ _ (Uid Int64 MergeSum) _)
dd_uid_merge_sum =
  uid prim (sum (con prims :> con prims))

test_statement_merge_sum :: TestT IO ()
test_statement_merge_sum = do
  target_mergeSum === toSql (Select (tableSchema dd_uid_merge_sum))
  target_mergeSum === toSql (Select (tableSchema dd_uid_merge_sum_manual))

data MergeProd =
  MergeProd { count :: Int, b :: Prod }
  deriving stock (Eq, Show, Generic)

dd_merge_prod :: Dd ('DdK _ _ MergeProd _)
dd_merge_prod =
  prod (prim :> merge (prod prims))

target_merge_prod :: Sql
target_merge_prod =
  [sql|select "count", "num", "name" from "merge_prod"|]

test_statement_merge_prod :: TestT IO ()
test_statement_merge_prod =
  target_merge_prod === toSql (Select (tableSchema dd_merge_prod))

test_statement :: TestTree
test_statement =
  testGroup "statement" [
    unitTest "order" test_statement_order,
    unitTest "merge sum" test_statement_merge_sum,
    unitTest "merge prod" test_statement_merge_prod
  ]
