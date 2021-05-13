module Polysemy.Hasql.Test.StatementTest where

import Polysemy.Db.Data.Column (Auto, Prim, PrimQuery, PrimaryKey, Sum, UidRep, Unique)
import Polysemy.Db.Data.Cond (LessOrEq)
import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.IdQuery (IdQuery)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Test (UnitTest, runTestAuto, unitTest, (===))
import Test.Tasty (TestTree, testGroup)

import Polysemy.Hasql.Column.DataColumn (tableStructure)
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.SqlCode (SqlCode(..))
import Polysemy.Hasql.Data.Where (Where(Where))
import qualified Polysemy.Hasql.Statement as Statement
import qualified Polysemy.Hasql.Table.Query.Insert as Query
import Polysemy.Hasql.Table.QueryTable (genQueryTable, queryTable)
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
    a :: Text,
    b :: NT,
    _c :: Maybe Double,
    sumField :: SumRec
  }
  deriving (Eq, Show, Generic)

data RecRep =
  RecRep {
    a :: Unique,
    b :: Auto,
    c :: Prim,
    sumField :: Sum SumRecRep
  }
  deriving (Eq, Show, Generic)

data Q1 =
  Q1 {
    a :: Text,
    c :: Double
  }
  deriving (Eq, Show, Generic)

test_selectStatement :: UnitTest
test_selectStatement =
  runTestAuto do
    target === stmtText
  where
    target =
      [qt|select "a", "b", "c", ("sum_field")."sum_index", ("sum_field")."d", ("sum_field")."r"."e", ("sum_field")."r"."f" from "rec" where "a" = $1 and "c" = $2|]
    SqlCode stmtText =
      Statement.selectWhereSql (queryTable @Q1 @Rec)

test_insertStatement :: UnitTest
test_insertStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|insert into "rec" ("a", "b", "c", "sum_field") values ($1, $2, $3, row($4, $5, row($6, $7)))|]
    stmtText :: SqlCode
    stmtText =
      Query.insert (tableStructure @Auto @Rec)

test_upsertStatement :: UnitTest
test_upsertStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|insert into "rec" ("a", "b", "c", "sum_field") values ($1, $2, $3, row($4, $5, row($6, $7))) on conflict ("a") do update set "a" = $1, "b" = $2, "c" = $3, "sum_field" = row($4, $5, row($6, $7))|]
    stmtText :: SqlCode
    stmtText =
      Statement.upsertSql (tableStructure @RecRep @Rec)

test_createStatement :: UnitTest
test_createStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|create table "rec" ("a" text unique not null, "b" bigint not null, "c" double precision, "sum_field" sum_rec not null)|]
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
      [qt|create table "user" ("id" bigint primary key, "name" text not null, "number" double precision not null)|]
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

test_queryWhereStatement :: UnitTest
test_queryWhereStatement =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [qt|($1 is null or "field2" <= $1) and ($2 is null or "field3" = $2) and "field4" = $3|]
    Where qw =
      queryWhere @QueryTestQType @QueryTestQ @QueryTestType @QueryTest

data SumData =
  SumData1 { int :: Int, double :: Double }
  |
  SumData2 { int :: Int, text :: Text, tixxt :: Text }
  deriving (Eq, Show, Generic)

data SumQ =
  SumQ1 { int :: Int, double :: Double }
  |
  SumQ2 { text :: Text, tixxt :: Text }
  deriving (Eq, Show, Generic)

data SumTable =
  SumTable { sum :: SumData }
  deriving (Eq, Show, Generic)

data SumTableQ =
  SumTableQ { sum :: SumQ }
  deriving (Eq, Show, Generic)

test_queryWhere_Sum :: UnitTest
test_queryWhere_Sum =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [qt|($1 is null or ("sum")."sum_index" = $1) and ($2 is null or ("sum")."sum_data1"."int" = $2) and ($3 is null or ("sum")."sum_data1"."double" = $3) and ($4 is null or ("sum")."sum_data2"."text" = $4) and ($5 is null or ("sum")."sum_data2"."tixxt" = $5)|]
    QueryTable _ _ (Where qw) =
      queryTable @SumTableQ @SumTable

test_queryWhere_Sum_Table :: UnitTest
test_queryWhere_Sum_Table =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [qt|"sum_index" = $1 and ($2 is null or ("sum_data1")."int" = $2) and ($3 is null or ("sum_data1")."double" = $3) and ($4 is null or ("sum_data2")."text" = $4) and ($5 is null or ("sum_data2")."tixxt" = $5)|]
    QueryTable _ _ (Where qw) =
      queryTable @SumQ @SumData

test_queryWhere_Sum_Prim :: UnitTest
test_queryWhere_Sum_Prim =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [qt|($1 is null or ("sum_data1")."int" = $1) or ($1 is null or ("sum_data2")."int" = $1)|]
    QueryTable _ _ (Where qw) =
      genQueryTable @(PrimQuery "int") @Auto @Int @SumData

data IDQTest =
  IDQTest {
    number :: Double
  }
  deriving (Eq, Show, Generic)

type IDQTestType =
  'Kind.Tree ('NamedField "IDQTest") '[] ('Kind.Prod IDQTest '[
    'Kind.Tree ('NamedField "number") '[] ('Kind.Prim Double)
  ])

test_IdQuery ::
  UnitTest
test_IdQuery =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [qt|"id" = $1|]
    QueryTable _ _ (Where qw) =
      genQueryTable @Auto @(UidRep Auto Auto) @(IdQuery Int) @(Uid Int IDQTest)

-- test_updateStatement :: UnitTest
-- test_updateStatement =
--   runTestAuto do target === stmtText
--   where
--     target =
--       [qt|update "rec" where "id" = $1 set "a" = $2, "b" = $3, "c" = $4, "sum_field" = row($5, $6, row($7, $8))|]
--     SqlCode stmtText =
--       Statement.updateSql (genQueryTable @(PrimQuery "id") @(UidRep Auto RecRep) @Int @(Uid Int Rec))

statementTests :: TestTree
statementTests =
  testGroup "statements" [
    unitTest "derive a select statement" test_selectStatement,
    unitTest "derive an insert statement" test_insertStatement,
    unitTest "derive an upsert statement" test_upsertStatement,
    unitTest "derive a create table statement" test_createStatement,
    unitTest "derive a create table statement with primary key" test_createPKStatement,
    unitTest "derive a where fragment" test_queryWhereStatement,
    unitTest "derive a where fragment for a sum with identical field names" test_queryWhere_Sum,
    unitTest "derive a where fragment for a sum as the table type" test_queryWhere_Sum_Table,
    unitTest "derive a where fragment for a sum with a prim query" test_queryWhere_Sum_Prim,
    unitTest "derive an IdQuery assignment" test_IdQuery
  ]
