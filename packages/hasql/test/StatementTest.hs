module StatementTest where

import GHC.TypeLits (Symbol)
import qualified Generics.SOP as SOP
import Generics.SOP.Type.Metadata (FieldInfo(FieldInfo))
import Prelude hiding (All)

import Polysemy.Db.Data.Column (Auto, Prim)
import Polysemy.Db.Data.SqlCode (SqlCode(..))
import qualified Polysemy.Db.Statement as Statement
import Polysemy.Db.Table.QueryFields (QueryFields)
import Polysemy.Db.Table.QueryTable (genQueryTable)
import Polysemy.Db.Table.TableStructure (genTableStructure)
import Polysemy.Db.SOP.Constraint (DataName, IsRecord)
import Polysemy.Db.Test (UnitTest, (===))
import Q1 (Q1)

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
    fieldThree :: Double
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
test_selectStatement = do
  target === unSqlCode stmtText
  where
    target =
      [i|select "field_one", "field_two", "field_three" from "rec" where "field_one" = $1 and "field_three" = $2|]
    stmtText :: SqlCode
    stmtText =
      Statement.selectWhereSql (genQueryTable @Rec @RecRep @Q1)

test_createStatement :: UnitTest
test_createStatement = do
  target === unSqlCode stmtText
  where
    target =
      [i|create table "rec" ("field_one" text, "field_two" bigint, "field_three" float8)|]
    stmtText :: SqlCode
    stmtText =
      Statement.createTableSql (genTableStructure @Rec @RecRep)
