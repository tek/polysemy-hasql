module Polysemy.Hasql.Test.StatementTest where

import Polysemy.Db.Data.Cond (LessOrEq (LessOrEq))
import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.Rep (Auto, Flatten, IdQuery, Prim, PrimQuery, PrimaryKey, Product, Sum, UidRep, Unique)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Polysemy.Hasql.Data.QueryTable (QueryTable (QueryTable))
import Polysemy.Hasql.Data.SqlCode (SqlCode (..))
import Polysemy.Hasql.Data.Where (Where (Where))
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.DataColumn (tableStructure)
import qualified Polysemy.Hasql.Table.Query.Insert as Query
import Polysemy.Hasql.Table.Schema (schema, schemaAuto)
import Polysemy.Hasql.Where (queryWhere)

data WithMaybe =
  WithMaybe {
     wm1 :: Int,
     wm2 :: Maybe Double
  }
  deriving (Eq, Show, Generic)

newtype NT =
  NT Int
  deriving (Eq, Show, Generic)

data SumRec =
  L { d :: Int }
  |
  R { e :: Text, f :: Double }
  deriving (Eq, Show, Generic)

data SumRecRep =
  LRep { d :: Prim }
  |
  RRep { e :: Prim, f :: Prim }
  deriving (Eq, Show, Generic)

data Rec =
  Rec {
    a_ :: Text,
    b :: NT,
    _c :: Maybe Double,
    sumField :: SumRec
  }
  deriving (Eq, Show, Generic)

data RecRep =
  RecRep {
    a_ :: Unique,
    b :: Auto,
    c :: Prim,
    sumField :: Sum SumRecRep
  }
  deriving (Eq, Show, Generic)

data Q1 =
  Q1 {
    a_ :: Text,
    c :: Double
  }
  deriving (Eq, Show, Generic)

test_selectStatement :: UnitTest
test_selectStatement =
  runTestAuto do
    target === stmtText
  where
    target =
      [text|select "a", "b", "c", ("sum_field")."ph_sum_index__sum_rec", ("sum_field")."d", ("sum_field")."r"."e", ("sum_field")."r"."f" from "rec" where "a" = $1 and "c" = $2|]
    SqlCode stmtText =
      Statement.selectWhereSql (schemaAuto @Q1 @Rec)

test_insertStatement :: UnitTest
test_insertStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [text|insert into "rec" ("a", "b", "c", "sum_field") values ($1, $2, $3, row($4, $5, row($6, $7)))|]
    stmtText :: SqlCode
    stmtText =
      Query.insert (tableStructure @Auto @Rec)

test_upsertStatement :: UnitTest
test_upsertStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [text|insert into "rec" ("a", "b", "c", "sum_field") values ($1, $2, $3, row($4, $5, row($6, $7))) on conflict ("a") do update set "a" = $1, "b" = $2, "c" = $3, "sum_field" = row($4, $5, row($6, $7))|]
    stmtText :: SqlCode
    stmtText =
      Statement.upsertSql (tableStructure @RecRep @Rec)

test_createStatement :: UnitTest
test_createStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [text|create table "rec" ("a" text unique not null, "b" bigint not null, "c" double precision, "sum_field" ph_type__sum_rec not null)|]
    stmtText :: SqlCode
    stmtText =
      Statement.createTableSql (tableStructure @RecRep @Rec)

newtype UserId =
  UserId { unUserId :: Int }
  deriving (Eq, Show, Generic)

data User =
  User {
    name :: Text,
    number :: Double
  }
  deriving (Eq, Show, Generic)

test_createPKStatement :: UnitTest
test_createPKStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [text|create table "user" ("id" bigint primary key, "name" text not null, "number" double precision not null)|]
    stmtText :: SqlCode
    stmtText =
      Statement.createTableSql (tableStructure @(UidRep PrimaryKey Auto) @(Uid UserId User))

data QueryTest =
  QueryTest {
    field1 :: Text,
    field2 :: Maybe Int,
    field3 :: Double,
    field4 :: Maybe Int
  }
  deriving (Eq, Show, Generic)

data QueryTestQ =
  QueryTestQ {
    field2 :: Maybe (LessOrEq Int),
    field3 :: Maybe Double,
    field4 :: Int
  }
  deriving (Eq, Show, Generic)

data QueryTest1Q =
  QueryTest1Q {
    field2 :: LessOrEq Int,
    field3 :: Maybe Double,
    field4 :: Int
  }
  deriving (Eq, Show, Generic)

type QueryTestType =
  'Kind.Tree ('NamedField "QueryTest") '[] ('Kind.Prod QueryTest [
    'Kind.Tree ('NamedField "field1") '[] ('Kind.Prim Text),
    'Kind.Tree ('NamedField "field2") '[] ('Kind.Prim (Maybe Int)),
    'Kind.Tree ('NamedField "field3") '[] ('Kind.Prim Double),
    'Kind.Tree ('NamedField "field4") '[] ('Kind.Prim (Maybe Int))
  ])

type QueryTestQType =
  'Kind.Tree ('NamedField "QueryTest") '[] ('Kind.Prod QueryTest [
    'Kind.Tree ('NamedField "field2") '[] ('Kind.Prim (Maybe (LessOrEq Int))),
    'Kind.Tree ('NamedField "field3") '[] ('Kind.Prim (Maybe Double)),
    'Kind.Tree ('NamedField "field4") '[] ('Kind.Prim Int)
  ])

type QueryTest1QType =
  'Kind.Tree ('NamedField "QueryTest") '[] ('Kind.Prod QueryTest [
    'Kind.Tree ('NamedField "field2") '[] ('Kind.Prim (LessOrEq Int)),
    'Kind.Tree ('NamedField "field3") '[] ('Kind.Prim (Maybe Double)),
    'Kind.Tree ('NamedField "field4") '[] ('Kind.Prim Int)
  ])

test_queryWhereStatement :: UnitTest
test_queryWhereStatement =
  runTestAuto do
    target === unSqlCode qw
    target1 === unSqlCode qw1
  where
    target =
      [text|($1 is null or "field2" <= $1) and ($2 is null or "field3" = $2) and "field4" = $3|]
    target1 =
      [text|"field2" <= $1 and ($2 is null or "field3" = $2) and "field4" = $3|]
    Where qw _ =
      queryWhere @Auto @QueryTestQType @QueryTestQ @QueryTestType @QueryTest
    Where qw1 _ =
      queryWhere @Auto @QueryTest1QType @QueryTest1Q @QueryTestType @QueryTest

test_queryWhereStatementInf :: UnitTest
test_queryWhereStatementInf =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [text|"field2" <= $1 and ($2 is null or "field3" = $2) and "field4" = $3|]
    QueryTable _ _ (Where qw _) =
      schema @Auto @Auto @QueryTest1Q @QueryTest

data SumData =
  SumData1 { int :: Int, double :: Double }
  |
  SumData2 { int :: Int, txt :: Text, tixxt :: Text }
  deriving (Eq, Show, Generic)

data SumQ =
  SumQ1 { int :: Int, double :: Double }
  |
  SumQ2 { txt :: Text, tixxt :: Text }
  deriving (Eq, Show, Generic)

data SumTable =
  SumTable { sum :: SumData }
  deriving (Eq, Show, Generic)

data SumTableQ =
  SumTableQ { sum :: SumQ }
  deriving (Eq, Show, Generic)

data SumUna =
  SumUnaL { unaL :: Int }
  |
  SumUnaR { unaR :: Double }
  deriving (Eq, Show, Generic)

data SumUnaExt =
  SumUnaExtL { unaL2 :: Int, unaL :: Int }
  |
  SumUnaExtR { unaR :: Double }
  deriving (Eq, Show, Generic)

test_createStatement_Sum :: UnitTest
test_createStatement_Sum =
  runTestAuto do
    target === stmtText
  where
    target =
      [text|create table "sum_data" ("ph_sum_index__sum_data" bigint not null, "sum_data1" ph_type__sum_data1 not null, "sum_data2" ph_type__sum_data2 not null)|]
    SqlCode stmtText =
      Statement.createTableSql (tableStructure @Auto @SumData)

test_queryWhere_Sum :: UnitTest
test_queryWhere_Sum =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [text|($1 is null or ("sum")."ph_sum_index__sum_data" = $1) and ($2 is null or ("sum")."sum_data1"."int" = $2) and ($3 is null or ("sum")."sum_data1"."double" = $3) and ($4 is null or ("sum")."sum_data2"."txt" = $4) and ($5 is null or ("sum")."sum_data2"."tixxt" = $5)|]
    QueryTable _ _ (Where qw _) =
      schemaAuto @SumTableQ @SumTable

test_queryWhere_Sum_Table :: UnitTest
test_queryWhere_Sum_Table =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [text|"ph_sum_index__sum_data" = $1 and ($2 is null or ("sum_data1")."int" = $2) and ($3 is null or ("sum_data1")."double" = $3) and ($4 is null or ("sum_data2")."txt" = $4) and ($5 is null or ("sum_data2")."tixxt" = $5)|]
    QueryTable _ _ (Where qw _) =
      schemaAuto @SumQ @SumData

test_queryWhere_Sum_Prim :: UnitTest
test_queryWhere_Sum_Prim =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [text|($1 is null or ("sum_data1")."int" = $1) or ($1 is null or ("sum_data2")."int" = $1)|]
    QueryTable _ _ (Where qw _) =
      schema @(PrimQuery "int") @Auto @Int @SumData

test_queryWhere_Sum_Unary :: UnitTest
test_queryWhere_Sum_Unary =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [text|"ph_sum_index__sum_una" = $1 and ($2 is null or "una_l" = $2) and ($3 is null or "una_r" = $3)|]
    QueryTable _ _ (Where qw _) =
      schemaAuto @SumUna @SumUna

test_queryWhere_Sum_UnaryQ :: UnitTest
test_queryWhere_Sum_UnaryQ =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [text|"ph_sum_index__sum_una_ext" = $1 and ($2 is null or ("sum_una_ext_l")."una_l" = $2) and ($3 is null or "una_r" = $3)|]
    QueryTable _ _ (Where qw _) =
      schemaAuto @SumUna @SumUnaExt

data IDQTest =
  IDQTest {
    number :: Double
  }
  deriving (Eq, Show, Generic)

type IDQTestType =
  'Kind.Tree ('NamedField "IDQTest") '[] ('Kind.Prod IDQTest '[
    'Kind.Tree ('NamedField "number") '[] ('Kind.Prim Double)
  ])

test_IdQuery :: UnitTest
test_IdQuery =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [text|"id" = $1|]
    QueryTable _ _ (Where qw _) =
      schema @IdQuery @(UidRep Auto Auto) @Int @(Uid Int IDQTest)

test_unitColumn :: UnitTest
test_unitColumn =
  runTestAuto do
    target === stmtText
  where
    target =
      [text|create table "idq_test" ("payload" ph_type__idq_test not null)|]
    SqlCode stmtText =
      Statement.createTableSql (tableStructure @Auto @(Uid () IDQTest))

data Flatty =
  Flatty {
    txt :: Text,
    double :: Double
  }
  deriving (Eq, Show, Generic)

data Sum1 =
  Sum1 {
    int1 :: Int,
    flatty :: Flatty
  }
  deriving (Eq, Show, Generic)

data Sum1Rep =
  Sum1Rep {
    int1 :: Auto,
    flatty :: Flatten Auto
  }
  deriving (Eq, Show, Generic)

data Sum2 =
  Sum2 {
    int2 :: Int,
    double :: Double
  }
  deriving (Eq, Show, Generic)

data UnaSum =
  UnaSum1 Sum1
  |
  UnaSum2 Sum2
  deriving (Eq, Show, Generic)

data UnaSumRep =
  UnaSumRep1 (Product Sum1Rep)
  |
  UnaSumRep2 Auto
  deriving (Eq, Show, Generic)

data FP =
  FP { double :: Double }
  deriving (Eq, Show, Generic)

test_queryWhere_Sum_Flatten :: UnitTest
test_queryWhere_Sum_Flatten =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [text|($1 is null or ("una_sum1")."double" = $1) or ($1 is null or ("una_sum2")."double" = $1)|]
    QueryTable _ _ (Where qw _) =
      schema @Auto @UnaSumRep @FP @UnaSum

statementTests :: TestTree
statementTests =
  testGroup "statements" [
    unitTest "derive a select statement" test_selectStatement,
    unitTest "derive an insert statement" test_insertStatement,
    unitTest "derive an upsert statement" test_upsertStatement,
    unitTest "derive a create table statement" test_createStatement,
    unitTest "derive a create table statement with primary key" test_createPKStatement,
    unitTest "derive a where fragment" test_queryWhereStatement,
    unitTest "derive a create table statement for a sum" test_createStatement_Sum,
    unitTest "derive a where fragment for a sum with identical field names" test_queryWhere_Sum,
    unitTest "derive a where fragment for a sum as the table type" test_queryWhere_Sum_Table,
    unitTest "derive a where fragment for a sum with a prim query" test_queryWhere_Sum_Prim,
    unitTest "derive a where fragment for a sum with a unary constructors" test_queryWhere_Sum_Unary,
    unitTest "derive a where fragment for a sum with a query with a unary constructors" test_queryWhere_Sum_UnaryQ,
    unitTest "derive an IdQuery assignment" test_IdQuery,
    unitTest "derive a unit column" test_unitColumn,
    unitTest "derive a where fragment for a sum and a nested flattened type" test_queryWhere_Sum_Flatten
  ]
