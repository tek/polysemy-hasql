module Polysemy.Db.Test.RepTest where

import Generics.SOP (Code)
import Prelude hiding (Enum)

import Polysemy.Db.Data.Column (Auto, Enum, Flatten, Prim, Sum)
import Polysemy.Db.Data.TableStructure (Column(Column), CompositeType(CompositeType), TableStructure(TableStructure))
import Polysemy.Hasql.Table.Columns (columns)
import Polysemy.Hasql.Table.Representation (
  ColumnCodes,
  ColumnCodess,
  ColumnRepss,
  EnumColumn,
  PrimColumn,
  ProdCode,
  ProdColumn,
  ProdTable,
  Rep,
  ReifyRepTable,
  SumTable,
  TableRep,
  )
import Polysemy.Test (UnitTest, runTestAuto, (===))

data Nummy =
  Onesy
  |
  Twosy
  |
  Threesy
  deriving (Eq, Show)

deriveGeneric ''Nummy

data Inside =
  Inside {
    ii :: Int,
    tt :: Text
  }
  deriving (Eq, Show)

deriveGeneric ''Inside

data InsideRep =
  InsideRep {
    ii :: Prim Auto,
    tt :: Prim Auto
  }
  deriving (Eq, Show)

deriveGeneric ''InsideRep

testInside ::
  ColumnCodes [Int, Inside] ~ [PrimColumn, ProdColumn [PrimColumn, PrimColumn]] =>
  Rep Inside ~ ProdTable (ProdCode (Code InsideRep)) =>
  ()
testInside =
  ()

data Summy =
  L { li :: Int, nummy :: Nummy }
  |
  M { mi :: Int, mt :: Text }
  |
  R { ri :: Int, rt :: Inside }
  deriving (Eq, Show)

deriveGeneric ''Summy

data SummyRep =
  LRep { li :: Prim Auto, nummy :: Enum Auto }
  |
  MRep { mi :: Prim Auto, mt :: Prim Auto }
  |
  RRep { ri :: Prim Auto, rt :: Flatten InsideRep }
  deriving (Eq, Show)

deriveGeneric ''SummyRep

data SummyRepAuto =
  LRepAuto { li :: Prim Auto, nummy :: Enum Auto }
  |
  MRepAuto { mi :: Prim Auto, mt :: Prim Auto }
  |
  RRepAuto { ri :: Prim Auto, rt :: Flatten (ProdColumn (ProdCode (Code InsideRep))) }
  deriving (Eq, Show)

deriveGeneric ''SummyRepAuto

type SummyCodess =
  [
    [PrimColumn, EnumColumn],
    [PrimColumn, PrimColumn],
    [PrimColumn, ProdColumn [PrimColumn, PrimColumn]]
  ]

type SummyRepss =
  [
    [Prim Auto, Enum Auto],
    [Prim Auto, Prim Auto],
    [Prim Auto, Flatten (ProdColumn [Prim Auto, Prim Auto])]
  ]

testSummy ::
  ColumnCodess (Code Summy) ~ SummyCodess =>
  TableRep (ColumnRepss (ColumnCodess (Code Summy))) ~ SumTable SummyRepss =>
  Rep Summy ~ SumTable (Code SummyRepAuto) =>
  ()
testSummy =
  ()

data Dat =
  Dat {
    id :: UUID,
    int :: Int,
    sum :: Summy
  }
  deriving (Eq, Show)

deriveGeneric ''Dat

data DatRepAuto =
  DatRepAuto {
    id :: Prim Auto,
    int :: Prim Auto,
    sum :: Sum (Code SummyRepAuto)
  }
  deriving (Eq, Show)

deriveGeneric ''DatRepAuto

data DatRep =
  DatRep {
    id :: Prim Auto,
    int :: Prim Auto,
    sum :: Sum SummyRep
  }
  deriving (Eq, Show)

deriveGeneric ''DatRep

testDat ::
  Rep Dat ~ ProdTable (ProdCode (Code DatRepAuto)) =>
  ReifyRepTable (Rep Dat) Dat ~ (ProdCode (Code DatRepAuto)) =>
  ()
testDat =
  ()

columnsExplicit :: [Column]
columnsExplicit =
  columns @DatRep @Dat

columnsImplicit :: [Column]
columnsImplicit =
  columns @(Rep Dat) @Dat

targetComposite :: CompositeType
targetComposite =
  CompositeType "summy" (Column "sum_index" "integer" def Nothing) [
    TableStructure "l" [Column "li" "bigint" def Nothing, Column "nummy" "text" def Nothing],
    TableStructure "m" [Column "mi" "bigint" def Nothing, Column "mt" "text" def Nothing],
    TableStructure "r" [
      Column "ri" "bigint" def Nothing,
      Column "ii" "bigint" def Nothing,
      Column "tt" "text" def Nothing
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
    pure testDat
    targetColumns === columnsImplicit
    columnsExplicit === columnsImplicit
