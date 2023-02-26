{-# options_ghc -Wno-partial-type-signatures -fconstraint-solver-iterations=10 #-}

module Sqel.Test.StatementTest where

import Data.Type.Equality ((:~:) (Refl))
import Generics.SOP (NP (..))
import Hedgehog (TestT, (===))
import Prelude hiding (sum)
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Class.MatchView (HasColumn)
import Sqel.Comp (Column, CompName (compName))
import Sqel.Data.Codec (FullCodec)
import Sqel.Data.Dd (
  Dd (Dd),
  DdInc (DdNest),
  DdK (DdK),
  DdSort (DdProd),
  DdStruct (DdComp),
  DdType,
  DdTypeSel,
  type (:>) ((:>)),
  )
import Sqel.Data.Mods (pattern NoMods)
import Sqel.Data.Order (Order (Desc))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sel (MkTSel (mkTSel), Sel (SelAuto), SelW (SelWAuto))
import Sqel.Data.Sql (Sql, sql, toSql)
import Sqel.Data.SqlFragment (Create (Create), Select (Select))
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)
import Sqel.Merge (merge)
import Sqel.PgType (MkTableSchema, tableSchema)
import Sqel.Prim (prim, primAs, primNewtypes, prims)
import Sqel.Product (prod, prodSel)
import Sqel.Query (checkQuery)
import Sqel.Query.Combinators (order)
import Sqel.ReifyCodec (ReifyCodec)
import Sqel.ReifyDd (ReifyDd)
import qualified Sqel.Sql.Select as Sql
import Sqel.Sum (ConColumn (con), con1, sum)
import Sqel.Test.Bug ()
import qualified Sqel.Type as T
import Sqel.Type (Prim, PrimNewtype, Prod, ProdPrimsNewtype, TypeSel, type (*>), type (>))
import Sqel.Uid (UidDd, uid)

newtype IntNt =
  IntNt { unIntNt :: Int }
  deriving stock (Eq, Show, Generic)

data Three =
  Three {
    a :: IntNt,
    b :: IntNt,
    c :: IntNt
  }
  deriving stock (Eq, Show, Generic)

type ThreeTableGen =
  ProdPrimsNewtype Three

type ThreeTable =
  Prod Three *> PrimNewtype "a" IntNt > PrimNewtype "b" IntNt > PrimNewtype "c" IntNt

test_prodGen :: TestT IO ()
test_prodGen =
  case Refl :: ThreeTable :~: ThreeTableGen of
    Refl -> unit

ddThree :: Dd ThreeTableGen
ddThree =
  prod primNewtypes

data Pro =
  Pro {
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

type ProdTable =
  Prod Pro *> (Prim "num" Int64 > Prim "name" Text)

ddPro :: Dd ProdTable
ddPro =
  prod prims

target_order :: Sql
target_order =
  [sql|select "num" from "dat" order by "num" desc|]

test_statement_order :: TestT IO ()
test_statement_order =
  target_order === Sql.selectWhere qs ts
  where
    ts :: TableSchema Dat
    ts = tableSchema (prod prim)
    qs :: QuerySchema Q Dat
    qs = checkQuery (prod (order Desc)) (prod prim)

target_mergeSum :: Sql
target_mergeSum =
  [sql|select "id", "sqel_sum_index__merge_sum", ("merge_sum1").num1, ("merge_sum1").name1, ("merge_sum2").num2,
       ("merge_sum2").name2 from "merge_sum"|]

target_create_mergeSum :: Sql
target_create_mergeSum =
  [sql|create table "merge_sum"
  ("id" bigint not null,
    "sqel_sum_index__merge_sum" bigint not null,
    "merge_sum1" sqel_type__merge_sum1 not null,
    "merge_sum2" sqel_type__merge_sum2 not null)
  |]

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
  target_create_mergeSum === toSql (Create (tableSchema dd_uid_merge_sum_manual))

data MergeProd =
  MergeProd { count :: Int, b :: Pro }
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

data Wrap a =
  Wrap { wrapped :: a, length :: Int64 }
  deriving stock (Eq, Show, Generic)

instance CompName a name => CompName (Wrap a) name where
  compName = compName @a

type WrapDd sa =
  TypeSel (DdTypeSel sa) (Prod (Wrap (DdType sa))) *> (
    T.Merge sa >
    Prim "length" Int64
  )

schema_higherOrder ::
  ∀ s0 table a sel mods s .
  s0 ~ 'DdK sel mods a s =>
  table ~ WrapDd s0 =>
  MkTSel (DdTypeSel s0) =>
  MkTableSchema table =>
  Dd s0 ->
  TableSchema (Wrap a)
schema_higherOrder wrapped =
  tableSchema dd
  where
    dd :: Dd table
    dd = Dd SelWAuto NoMods (DdComp mkTSel DdProd DdNest fields)
    fields = merge wrapped :* primAs @"length" :* Nil

target_higherOrder :: Sql
target_higherOrder =
  [sql|select "num", "name", "length" from "pro"|]

test_statement_higherOrder :: TestT IO ()
test_statement_higherOrder =
  target_higherOrder === toSql (Select (schema_higherOrder ddPro))

data Merge1 =
  One { one :: Pro }
  |
  Two { two :: Pro }
  deriving stock (Eq, Show, Generic)

data QHo = QHo { name :: Text }
  deriving stock (Eq, Show, Generic)

data QWrap = QWrap { two :: QHo }
  deriving stock (Eq, Show, Generic)

statement_query_higherOrder ::
  ∀ a s0 table mods s .
  s0 ~ 'DdK 'SelAuto mods a s =>
  MkTSel (DdTypeSel s0) =>
  table ~ UidDd (Prim "id" Int64) (WrapDd s0) =>
  MkTableSchema table =>
  HasColumn ["two", "name"] Text table =>
  Dd s0 ->
  Sql
statement_query_higherOrder wrapped =
  Sql.selectWhere qs ts
  where
    ts :: TableSchema (Uid Int64 (Wrap a))
    ts = tableSchema dd
    qs :: QuerySchema QWrap (Uid Int64 (Wrap a))
    qs = checkQuery q dd
    q = prod (prod prim)
    dd :: Dd table
    dd = uid prim pro
    pro = Dd SelWAuto NoMods (DdComp mkTSel DdProd DdNest fields)
    fields = merge wrapped :* primAs @"length" :* Nil

ddMerge1 :: Dd ('DdK _ _ Merge1 _)
ddMerge1 = sum (con1 ddPro :> con1 ddPro)

target_merge_query_higherOrder :: Sql
target_merge_query_higherOrder =
  [sql|select "id", "sqel_sum_index__merge1", ("one").num, ("one").name, ("two").num, ("two").name, "length"
       from "merge1" where ((("two")."name" = $1))|]

test_statement_merge_query_higherOrder :: TestT IO ()
test_statement_merge_query_higherOrder =
  target_merge_query_higherOrder === statement_query_higherOrder ddMerge1

ddHigherOrder2 ::
  ∀ s merged .
  merged ~ T.Merge s =>
  MkTSel (DdTypeSel s) =>
  Column (DdType s) "wrapped" merged merged =>
  Dd s ->
  Dd (UidDd (Prim "id" Int64) (WrapDd s))
ddHigherOrder2 wrapped =
  uid prim (prodSel @(DdTypeSel s) (merge wrapped :> prim))

ddUidWrapPro :: Dd (UidDd (Prim "id" Int64) (WrapDd ProdTable))
ddUidWrapPro =
  ddHigherOrder2 ddPro

higherOrder2 ::
  ∀ a s merged .
  merged ~ T.Merge s =>
  MkTSel (DdTypeSel s) =>
  Column a "wrapped" merged merged =>
  ReifyDd merged =>
  ReifyCodec FullCodec merged a =>
  Dd s ->
  Sql
higherOrder2 wrapped =
  toSql (Select ts)
  where
    ts :: TableSchema (Wrap a)
    ts = tableSchema dd
    dd = prodSel @(DdTypeSel s) (merge wrapped :> prim)

test_higherOrder2 :: TestT IO ()
test_higherOrder2 =
  [sql|select "num", "name", "length" from "pro"|] === higherOrder2 ddPro

data NaNu =
  Na { name :: Text }
  |
  Nu Int64
  deriving stock (Eq, Show, Generic)

ddNaNu :: Dd ('DdK _ _ NaNu _)
ddNaNu =
  sum (con1 prim :> con1 prim)

statement_con1 :: Sql
statement_con1 =
  toSql (Select (tableSchema ddNaNu))

test_statement_con1 :: TestT IO ()
test_statement_con1 =
  [sql|select "sqel_sum_index__na_nu", "name", "nu" from "na_nu"|] === statement_con1

newtype TextNt =
  TextNt { unTextNt :: Text }
  deriving stock (Eq, Show, Generic)
  deriving newtype (IsString, Ord)

test_statement :: TestTree
test_statement =
  testGroup "statement" [
    unitTest "order" test_statement_order,
    unitTest "merge sum" test_statement_merge_sum,
    unitTest "merge prod" test_statement_merge_prod,
    unitTest "higher-order merge statement" test_statement_higherOrder,
    unitTest "higher-order double merge query" test_statement_merge_query_higherOrder,
    unitTest "higher-order with new product class" test_higherOrder2,
    unitTest "unary con with record and positional fields" test_statement_con1
  ]
