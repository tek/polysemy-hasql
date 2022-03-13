{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.WhereTest where

import Fcf (Eval)
import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField))
import Polysemy.Db.Data.Rep (Auto, Flatten, Product)
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Tree.Table (DbQueryRoot, TableRoot)
import Polysemy.Hasql.Where (Where)
import Polysemy.Hasql.Where.Cond (MatchTable, QCond (SumPrimCond))
import Polysemy.Hasql.Where.FlatFields (FieldPath (FieldPath), FlatRoot)
import Polysemy.Hasql.Where.Segment (Segment (ConSegment, FieldSegment, SumIndexSegment))

data Flatty =
  Flatty {
    txt :: Text,
    double :: Double
  }
  deriving stock (Eq, Show, Generic)

data Sum1 =
  Sum1 {
    int1 :: Int,
    flatty :: Flatty
  }
  deriving stock (Eq, Show, Generic)

data Sum1Rep =
  Sum1Rep {
    int1 :: Auto,
    flatty :: Flatten Auto
  }
  deriving stock (Eq, Show, Generic)

data Sum2 =
  Sum2 {
    int2 :: Int,
    double :: Double
  }
  deriving stock (Eq, Show, Generic)

data UnaSum =
  UnaSum1 Sum1
  |
  UnaSum2 Sum2
  deriving stock (Eq, Show, Generic)

data UnaSumRep =
  UnaSum1Rep (Product Sum1Rep)
  |
  UnaSum2Rep Auto
  deriving stock (Eq, Show, Generic)

data FP =
  FP { double :: Double }
  deriving stock (Eq, Show, Generic)

type Seg1With rest =
  [
    'ConSegment 0 ('NamedField "UnaSum1") 'True,
    'FieldSegment ('NumberedField "UnaSum1" 1)
  ] ++ rest

type Seg1 =
  Seg1With '[ 'FieldSegment ('NamedField "double")]

type Seg2With last =
  [
    'ConSegment 1 ('NamedField "UnaSum2") 'True,
    'FieldSegment ('NumberedField "UnaSum2" 1),
    last
  ]

type Seg2 =
  Seg2With ('FieldSegment ('NamedField "double"))

derivation ::
  TableRoot UnaSumRep UnaSum dTree =>
  DbQueryRoot Auto FP UnaSum qTree =>
  dFlat ~ Eval (FlatRoot dTree) =>
  dFlat ~ '[
    'FieldPath '[ 'SumIndexSegment UnaSum ] Int,
    'FieldPath (Seg1With '[ 'FieldSegment ('NamedField "int1")]) Int,
    'FieldPath (Seg1With '[ 'FieldSegment ('NamedField "txt")]) Text,
    'FieldPath (Seg1With '[ 'FieldSegment ('NamedField "double")]) Double,
    'FieldPath (Seg2With ('FieldSegment ('NamedField "int2"))) Int,
    'FieldPath (Seg2With ('FieldSegment ('NamedField "double"))) Double
  ] =>
  qFlat ~ Eval (FlatRoot qTree) =>
  qFlat ~ '[
    'FieldPath '[ 'FieldSegment ('NamedField "double")] Double
  ] =>
  fields ~ MatchTable qTree dTree =>
  fields ~ '[ 'SumPrimCond (Maybe Double) Double '[Seg1, Seg2]] =>
  Where Auto qTree FP dTree UnaSum =>
  ()
derivation =
  ()

test_where_Flatten_Sum :: UnitTest
test_where_Flatten_Sum =
  pure derivation

sumQueryDerivation ::
  TableRoot UnaSumRep UnaSum dTree =>
  DbQueryRoot Auto FP UnaSum qTree =>
  dFlat ~ Eval (FlatRoot dTree) =>
  dFlat ~ '[
    'FieldPath '[ 'SumIndexSegment UnaSum ] Int,
    'FieldPath (Seg1With '[ 'FieldSegment ('NamedField "int1")]) Int,
    'FieldPath (Seg1With '[ 'FieldSegment ('NamedField "txt")]) Text,
    'FieldPath (Seg1With '[ 'FieldSegment ('NamedField "double")]) Double,
    'FieldPath (Seg2With ('FieldSegment ('NamedField "int2"))) Int,
    'FieldPath (Seg2With ('FieldSegment ('NamedField "double"))) Double
  ] =>
  qFlat ~ Eval (FlatRoot qTree) =>
  qFlat ~ '[
    'FieldPath '[ 'FieldSegment ('NamedField "double")] Double
  ] =>
  fields ~ MatchTable qTree dTree =>
  fields ~ '[ 'SumPrimCond (Maybe Double) Double '[Seg1, Seg2]] =>
  Where Auto qTree FP dTree UnaSum =>
  ()
sumQueryDerivation =
  ()

test_where_sumQuery :: UnitTest
test_where_sumQuery =
  pure sumQueryDerivation
