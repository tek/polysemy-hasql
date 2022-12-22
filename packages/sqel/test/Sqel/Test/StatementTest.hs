{-# options_ghc -Wno-partial-type-signatures -fconstraint-solver-iterations=100 #-}

module Sqel.Test.StatementTest where

import Data.Type.Equality ((:~:) (Refl))
import Generics.SOP (NP (..))
import Hedgehog (TestT, (===))
import Prelude hiding (sum)
import Test (unitTest)
import Test.Tasty (TestTree, testGroup)

import Sqel.Class.MatchView (HasColumn)
import Sqel.Data.Dd (
  DbTypeName (dbTypeName),
  Dd (Dd),
  DdInc (DdNest),
  DdK (DdK),
  DdStruct (DdComp),
  DdType,
  DdTypeName,
  DdVar (DdProd),
  Struct (Comp),
  type (:>) ((:>)),
  )
import Sqel.Data.Mods (pattern NoMods)
import Sqel.Data.Order (Order (Desc))
import Sqel.Data.QuerySchema (QuerySchema)
import Sqel.Data.Sel (MkSel (mkSel), Sel (SelSymbol), SelW (SelWAuto))
import Sqel.Data.Select (Select (Select))
import Sqel.Data.Sql (Sql, sql, toSql)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Data.Uid (Uid)
import Sqel.Merge (Merge, merge)
import Sqel.PgType (MkTableSchema, tableSchema)
import Sqel.Prim (prim, primAs, prims)
import Sqel.Product (prod)
import Sqel.Query (checkQuery)
import Sqel.Query.Combinators (order)
import qualified Sqel.Sql.Select as Sql
import Sqel.Sum (con, con1, sum)
import qualified Sqel.Type as T
import Sqel.Type (Prim, Prod, ProdPrims, TypeName, type (*>), type (>))
import Sqel.Uid (UidDd, uid)

data Three =
  Three {
    a :: Int,
    b :: Int,
    c :: Int
  }
  deriving stock (Eq, Show, Generic)

type ThreeTableGen =
  ProdPrims Three

type ThreeTable =
  Prod Three *> Prim "a" Int > Prim "b" Int > Prim "c" Int

test_prodGen :: TestT IO ()
test_prodGen =
  case Refl :: ThreeTable :~: ThreeTableGen of
    Refl -> unit

ddThree :: Dd ThreeTableGen
ddThree =
  prod prims

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

instance DbTypeName a name => DbTypeName (Wrap a) name where
  dbTypeName = dbTypeName @a

type WrapDd sa =
  TypeName (DdTypeName sa) (Prod (Wrap (DdType sa))) *> (
    T.Merge sa >
    Prim "length" Int64
  )

schema_higherOrder ::
  ∀ s0 table a sel mods s .
  s0 ~ 'DdK sel mods a s =>
  table ~ WrapDd s0 =>
  Merge a s0 (T.Merge s0) =>
  MkSel ('SelSymbol (DdTypeName s0)) =>
  MkTableSchema table =>
  Dd s0 ->
  TableSchema (Wrap a)
schema_higherOrder wrapped =
  tableSchema dd
  where
    dd :: Dd table
    dd = Dd SelWAuto NoMods (DdComp mkSel DdProd DdNest fields)
    fields = merge @a wrapped :* primAs @"length" :* Nil

target_higherOrder :: Sql
target_higherOrder =
  [sql|select "num", "name", "length" from "pro"|]

test_statement_higherOrder :: TestT IO ()
test_statement_higherOrder =
  target_higherOrder === toSql (Select (schema_higherOrder ddPro))

statement_query_higherOrder ::
  ∀ a s0 table sel mods tsel c i s .
  s0 ~ 'DdK sel mods a ('Comp ('SelSymbol tsel) c i s) =>
  table ~ UidDd (Prim "id" Int64) (WrapDd s0) =>
  KnownSymbol tsel =>
  DbTypeName a tsel =>
  MkTableSchema table =>
  HasColumn ["two", "name"] Text table =>
  Dd s0 ->
  Sql
statement_query_higherOrder wrapped@(Dd _ _ (DdComp tsel _ _ _)) =
  Sql.selectWhere qs ts
  where
    ts :: TableSchema (Uid Int64 (Wrap a))
    ts = tableSchema dd
    qs :: QuerySchema QWrap (Uid Int64 (Wrap a))
    qs = checkQuery (prod (prod prim)) dd
    dd :: Dd table
    dd = uid prim pro
    pro = Dd SelWAuto NoMods (DdComp tsel DdProd DdNest fields)
    fields = merge wrapped :* primAs @"length" :* Nil

data Merge1 =
  One { one :: Pro }
  |
  Two { two :: Pro }
  deriving stock (Eq, Show, Generic)

data QHo = QHo { name :: Text }
  deriving stock (Eq, Show, Generic)

data QWrap = QWrap { two :: QHo }
  deriving stock (Eq, Show, Generic)

ddMerge1 :: Dd ('DdK _ _ Merge1 _)
ddMerge1 = sum (con1 ddPro :> con1 ddPro)

target_merge_query_higherOrder :: Sql
target_merge_query_higherOrder =
  [sql|select "id", "ph_sum_index__merge1", ("one").num, ("one").name, ("two").num, ("two").name, "length"
       from "merge1" where ((("two")."name" = $1))|]

test_statement_merge_query_higherOrder :: TestT IO ()
test_statement_merge_query_higherOrder =
  target_merge_query_higherOrder === statement_query_higherOrder ddMerge1

test_statement :: TestTree
test_statement =
  testGroup "statement" [
    unitTest "order" test_statement_order,
    unitTest "merge sum" test_statement_merge_sum,
    unitTest "merge prod" test_statement_merge_prod,
    unitTest "higher-order merge statement" test_statement_higherOrder,
    unitTest "higher-order double merge query" test_statement_merge_query_higherOrder
  ]
