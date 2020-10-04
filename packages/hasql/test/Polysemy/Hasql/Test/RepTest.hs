module Polysemy.Hasql.Test.RepTest where

import Generics.SOP.GGP (GCode)
import Path (Abs, File, Path)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, NewtypePrim, Prim, Sum)
import Polysemy.Db.Data.ColumnParams (notNull)
import Polysemy.Db.Data.TableStructure (Column(Column), CompositeType(CompositeType), TableStructure(TableStructure))
import Polysemy.Hasql.Table.Columns (columns)
import Polysemy.Hasql.Table.Representation (
  ColumnCode,
  ColumnCodes,
  ColumnCodess,
  NestedSum,
  ProdCode,
  ProdColumn,
  ProdTable,
  ReifyRepTable,
  Rep,
  SumColumn,
  )
import Polysemy.Test (UnitTest, runTestAuto, (===))

newtype Newty =
  Newty Text
  deriving (Eq, Show, Generic)

data Nummy =
  Onesy
  |
  Twosy
  |
  Threesy
  deriving (Eq, Show, Generic)

data Inside =
  Inside {
    ii :: Path Abs File,
    tt :: Maybe Newty
  }
  deriving (Eq, Show, Generic)

data InsideRepAuto =
  InsideRepAuto {
    ii :: Prim Auto,
    tt :: NewtypePrim Auto
  }
  deriving (Show, Generic)

data InsideRep =
  InsideRep {
    ii :: Prim Auto,
    tt :: NewtypePrim Auto
  }
  deriving (Show, Generic)

testInside ::
  ColumnCodes '[Int] ~ '[Prim Auto] =>
  ColumnCode Inside ~ Flatten (ProdColumn [Prim Auto, NewtypePrim Auto]) =>
  ColumnCodes [Int, Inside] ~ [Prim Auto, Flatten (ProdColumn [Prim Auto, NewtypePrim Auto])] =>
  Rep Inside ~ ProdTable (ProdCode (GCode InsideRepAuto)) =>
  ()
testInside =
  ()

data Summy =
  L { li :: Int, nummy :: Nummy }
  |
  M { mi :: Int, mt :: Text }
  |
  R { ri :: Int, rt :: Inside }
  deriving (Eq, Show, Generic)

data SummyRep =
  LRep { li :: Prim Auto, nummy :: Enum Auto }
  |
  MRep { mi :: Prim Auto, mt :: Prim Auto }
  |
  RRep { ri :: Prim Auto, rt :: Flatten InsideRep }
  deriving (Eq, Show, Generic)

data SummyRepAuto =
  LRepAuto { li :: Prim Auto, nummy :: Enum Auto }
  |
  MRepAuto { mi :: Prim Auto, mt :: Prim Auto }
  |
  RRepAuto { ri :: Prim Auto, rt :: Flatten (ProdColumn (ProdCode (GCode InsideRepAuto))) }
  deriving (Eq, Show, Generic)

type SummyCodess =
  [
    [Prim Auto, Enum Auto],
    [Prim Auto, Prim Auto],
    [Prim Auto, Flatten (ProdColumn [Prim Auto, NewtypePrim Auto])]
  ]

testSummy ::
  ColumnCodess (GCode Summy) ~ SummyCodess =>
  ColumnCode Summy ~ Sum (SumColumn (NestedSum (GCode SummyRepAuto))) =>
  ()
testSummy =
  ()

data SumField =
  SumField {
    id :: UUID,
    int :: Int,
    sum :: Summy
  }
  deriving (Eq, Show, Generic)

data SumFieldRepAuto =
  SumFieldRepAuto {
    id :: Prim Auto,
    int :: Prim Auto,
    sum :: Sum (SumColumn (NestedSum (GCode SummyRepAuto)))
  }
  deriving (Show, Generic)

data SumFieldRep =
  SumFieldRep {
    id :: Prim Auto,
    int :: Prim Auto,
    sum :: Sum SummyRep
  }
  deriving (Show, Generic)

testSumField ::
  Rep SumField ~ ProdTable (ProdCode (GCode SumFieldRepAuto)) =>
  ReifyRepTable (Rep SumField) SumField ~ ProdColumn (ProdCode (GCode SumFieldRepAuto)) =>
  (ReifyRepTable SumFieldRep SumField) ~ ProdColumn (ProdCode (GCode SumFieldRepAuto)) =>
  ()
testSumField =
  ()

columnsExplicit :: [Column]
columnsExplicit =
  columns @SumFieldRep @SumField

columnsImplicit :: [Column]
columnsImplicit =
  columns @(Rep SumField) @SumField

targetComposite :: CompositeType
targetComposite =
  CompositeType "summy" (Column "sum_index" "integer" def Nothing) [
    TableStructure "l" [Column "li" "bigint" def Nothing, Column "nummy" "text" def Nothing],
    TableStructure "m" [Column "mi" "bigint" def Nothing, Column "mt" "text" def Nothing],
    TableStructure "r" [
      Column "ri" "bigint" def Nothing,
      Column "ii" "text" def Nothing,
      Column "tt" "text" def { notNull = False } Nothing
    ]
  ]

targetColumns :: [Column]
targetColumns =
  [
    Column "id" "uuid" def Nothing,
    Column "int" "bigint" def Nothing,
    Column "sum" "summy" def (Just targetComposite)
  ]

test_rep :: UnitTest
test_rep = do
  runTestAuto do
    pure testInside
    pure testSummy
    pure testSumField
    targetColumns === columnsImplicit
    columnsExplicit === columnsImplicit
