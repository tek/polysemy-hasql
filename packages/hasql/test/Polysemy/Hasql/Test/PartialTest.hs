{-# options_ghc -Wno-all -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.PartialTest where

import Polysemy.Db.Data.FieldId (FieldId(NamedField))
import Polysemy.Db.Data.PartialField (PartialField)
import Polysemy.Db.Data.PartialFields (PartialFields)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Partial (field, partial, updatePartial, (.>))
import Polysemy.Db.Partial.Tree (partialTree, (..>))
import Polysemy.Db.Tree.Data (DataTag, dataTree)
import Polysemy.Db.Tree.Partial (PartialTag, partialTree', updatePartialTree', (...>))
import qualified Polysemy.Db.Type.Data.Tree as Type
import Polysemy.Db.Type.Data.Tree (Tree)
import Polysemy.Test (UnitTest, runTestAuto, (===))

data Dat =
  Dat {
    int :: Int,
    double :: Double
  }
  deriving (Eq, Show, Generic)

partialUpdate :: PartialFields Dat
partialUpdate =
  partial @Dat .> field @"int" 5 .> field @"double" 17.5

test_partialUpdate :: UnitTest
test_partialUpdate =
  runTestAuto do
    target === updatePartial partialUpdate record

type TreeType =
  'Kind.Tree ('NamedField "Dat") () ('Kind.Prod Dat '[
    'Kind.Tree ('NamedField "int") () ('Kind.Prim Int),
    'Kind.Tree ('NamedField "double") () ('Kind.Prim Double)
  ])

type DataType =
  'Kind.Tree ('NamedField "Dat") () ('Kind.Prod Dat '[
    'Kind.Tree ('NamedField "int") () ('Kind.Prim Int),
    'Kind.Tree ('NamedField "double") () ('Kind.Prim Double)
  ])

datTree :: Type.Tree DataTag Identity DataType
datTree =
  dataTree @Dat record

record :: Dat
record =
  Dat 9 5

target :: Dat
target =
  Dat 5 17.5

partialUpdateTree' :: Tree PartialTag PartialField TreeType
partialUpdateTree' =
  partialTree' @Dat ...> field @"int" (5 :: Int) ...> field @"double" (17.5 :: Double)

test_partialColumn :: UnitTest
test_partialColumn =
  runTestAuto do
    target === updatePartialTree' partialUpdateTree' record
