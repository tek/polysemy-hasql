module Polysemy.Db.Test.StatementTest where

import qualified Generics.SOP as SOP
import Generics.SOP.Type.Metadata (FieldInfo(FieldInfo))
import Prelude hiding (All)

import Polysemy.Db.Data.Column (Auto, Prim, Sum)
import Polysemy.Db.SOP.Constraint (DataName, IsRecord)
import Polysemy.Hasql.Data.SqlCode (SqlCode(..))
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.QueryFields (QueryFields)
import Polysemy.Hasql.Table.QueryTable (genQueryTable)
import Polysemy.Hasql.Table.TableStructure (genTableStructure)
import Polysemy.Test (UnitTest, runTestAuto, (===))

data WithMaybe =
  WithMaybe {
     wm1 :: Int,
     wm2 :: Maybe Double
  }
  deriving (Eq, Show, Generic)

instance SOP.Generic WithMaybe
instance SOP.HasDatatypeInfo WithMaybe

derivation ::
  ∀ names (columns :: [*]) (name :: Symbol) .
  QueryFields '[ 'FieldInfo "f1", 'FieldInfo "f3"] '[Text, Double] '[ 'FieldInfo "f1", 'FieldInfo "f2", 'FieldInfo "f3"] '[Text, Int, Double] =>
  QueryFields '[ 'FieldInfo "f2", 'FieldInfo "f3"] '[Int, Maybe Double] '[ 'FieldInfo "f1", 'FieldInfo "f2", 'FieldInfo "f3"] '[Text, Int, Maybe Double] =>
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

instance SOP.Generic NT

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
    a :: Prim Auto,
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
      Statement.selectWhereSql (genQueryTable @RecRep @Q1 @Rec)

test_createStatement :: UnitTest
test_createStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|create table "rec" ("a" text not null, "b" bigint not null, "c" double precision, "sum_field" sum_rec not null)|]
    stmtText :: SqlCode
    stmtText =
      Statement.createTableSql (genTableStructure @RecRep @Rec)
