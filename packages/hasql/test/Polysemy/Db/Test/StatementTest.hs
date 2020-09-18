module Polysemy.Db.Test.StatementTest where

import GHC.TypeLits (Symbol)
import qualified Generics.SOP as SOP
import Generics.SOP.Type.Metadata (FieldInfo(FieldInfo))
import Prelude hiding (All)

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Hasql.Data.SqlCode (SqlCode(..))
import Polysemy.Db.SOP.Constraint (DataName, IsRecord)
import qualified Polysemy.Hasql.Statement as Statement
import Polysemy.Hasql.Table.QueryFields (QueryFields)
import Polysemy.Hasql.Table.QueryTable (genQueryTable)
import Polysemy.Hasql.Table.TableStructure (genTableStructure)
import Polysemy.Db.Test.Q1 (Q1)
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

data Rec =
  Rec {
    fieldOne :: Text,
    fieldTwo :: NT,
    fieldThree :: Maybe Double
  }
  deriving (Eq, Show)

deriveGeneric ''Rec

data RecRep =
  RecRep {
    fieldOne :: Prim Auto,
    fieldTwo :: Prim Auto,
    fieldThree :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''RecRep

test_selectStatement :: UnitTest
test_selectStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|select "field_one", "field_two", "field_three" from "rec" where "field_one" = $1 and "field_three" = $2|]
    stmtText :: SqlCode
    stmtText =
      Statement.selectWhereSql (genQueryTable @RecRep @Q1 @Rec)

test_createStatement :: UnitTest
test_createStatement =
  runTestAuto do
    target === unSqlCode stmtText
  where
    target =
      [qt|create table "rec" ("field_one" text not null, "field_two" bigint not null, "field_three" double precision)|]
    stmtText :: SqlCode
    stmtText =
      Statement.createTableSql (genTableStructure @RecRep @Rec)
