module Polysemy.Hasql.Test.StatementTest where

import Generics.SOP.Type.Metadata (FieldInfo(FieldInfo))
import Prelude hiding (All)

import Polysemy.Db.Data.Column (Auto, NewtypePrim, Prim, Sum, UidRep, Unique)
import Polysemy.Db.Data.Cond (LessOrEq)
import Polysemy.Db.Data.Uid (Uid)
import Polysemy.Db.SOP.Constraint (DataName, IsRecord)
import Polysemy.Hasql.Data.QueryWhere (QueryWhere(QueryWhere))
import Polysemy.Hasql.Data.SqlCode (SqlCode(..))
import qualified Polysemy.Hasql.Statement as Statement
import qualified Polysemy.Hasql.Table.Query.Insert as Query
import Polysemy.Hasql.Table.Query.Where (queryWhere')
import Polysemy.Hasql.Table.QueryTable (queryTable)
import Polysemy.Hasql.Table.Representation (Rep)
import Polysemy.Hasql.Table.TableStructure (genTableStructure, tableStructure)
import Polysemy.Test (UnitTest, runTestAuto, (===))

data WithMaybe =
  WithMaybe {
     wm1 :: Int,
     wm2 :: Maybe Double
  }
  deriving (Eq, Show, Generic)

derivation ::
  ∀ names (columns :: [*]) (name :: Symbol) .
  IsRecord WithMaybe columns name names =>
  DataName WithMaybe =>
  IO ()
derivation =
  unit

test_derivation :: IO ()
test_derivation =
  derivation

newtype NT =
  NT Int
  deriving (Eq, Show, Generic)

data SumRec =
  L { d :: Int }
  |
  R { e :: Text, f :: Double }
  deriving (Eq, Show, Generic)

data SumRecRep =
  LRep { d :: Prim Auto }
  |
  RRep { e :: Prim Auto, f :: Prim Auto }
  deriving (Eq, Show, Generic)

data Rec =
  Rec {
    a :: Text,
    b :: NT,
    c :: Maybe Double,
    sumField :: SumRec
  }
  deriving (Eq, Show, Generic)

data RecRep =
  RecRep {
    a :: Prim Unique,
    b :: Prim Auto,
    c :: Prim Auto,
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
    target === unSqlCode stmtText
  where
    target =
      [qt|select "a", "b", "c", ("sum_field").sum_index, ("sum_field")."l"."d", ("sum_field")."r"."e", ("sum_field")."r"."f" from "rec" where "a" = $1 and "c" = $2|]
    stmtText :: SqlCode
    stmtText =
      Statement.selectWhereSql (queryTable @Q1 @Rec)

test_insertStatement :: UnitTest
test_insertStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|insert into "rec" ("a", "b", "c", "sum_field") values ($1, $2, $3, row($4, row($5), row($6, $7)))|]
    stmtText :: SqlCode
    stmtText =
      Query.insert (tableStructure @Rec)

test_upsertStatement :: UnitTest
test_upsertStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|insert into "rec" ("a", "b", "c", "sum_field") values ($1, $2, $3, row($4, row($5), row($6, $7))) on conflict ("a") do update set a = $1, b = $2, c = $3, sum_field = row($4, row($5), row($6, $7))|]
    stmtText :: SqlCode
    stmtText =
      Statement.upsertSql (genTableStructure @RecRep @Rec)

test_createStatement :: UnitTest
test_createStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|create table "rec" ("a" text unique not null, "b" bigint not null, "c" double precision, "sum_field" sum_rec not null)|]
    stmtText :: SqlCode
    stmtText =
      Statement.createTableSql (genTableStructure @RecRep @Rec)

newtype UserId =
  UserId { unUserId :: Int }
  deriving (Eq, Show, Generic)

data User =
  User {
    name :: Text,
    number :: Int
  }
  deriving (Eq, Show, Generic)

test_createPKStatement :: UnitTest
test_createPKStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|create table "user" ("id" bigint primary key, "name" text not null, "number" bigint not null)|]
    stmtText :: SqlCode
    stmtText =
      Statement.createTableSql (genTableStructure @(UidRep (NewtypePrim Auto) User) @(Uid UserId User))

data QueryTest =
  QueryTest {
    field1 :: Text,
    field2 :: Int,
    field3 :: Double,
    field4 :: Maybe Int
  }
  deriving (Eq, Show, Generic)

data QueryTestQ =
  QueryTestQ {
    field2 :: LessOrEq Int,
    field3 :: Maybe Double,
    field4 :: Int
  }
  deriving (Eq, Show, Generic)

test_queryWhereStatement :: UnitTest
test_queryWhereStatement =
  runTestAuto do
    target === unSqlCode qw
  where
    target =
      [qt|"field2" <= $1 and ($2 is null or "field3" = $2) and "field4" = $3|]
    QueryWhere qw =
      queryWhere' @(Rep QueryTest) @QueryTest @QueryTestQ
