module Polysemy.Hasql.Test.PartialTest where

import Polysemy.Db.Data.PartialFields (PartialFieldsT)
import Polysemy.Db.Data.Uid (Uid(Uid))
import Polysemy.Db.Partial (field, partial, updatePartial, (.>))
import Polysemy.Test (UnitTest, runTestAuto, (===))

data Dat =
  Dat {
    int :: Int,
    double :: Double,
    texts :: [Text]
  }
  deriving (Eq, Show, Generic)

partialUpdate :: PartialFieldsT Dat
partialUpdate =
  partial @Dat .> field @"int" 5 .> field @"double" 17.5

partialUpdateUid :: Uid Int (PartialFieldsT Dat)
partialUpdateUid =
  Uid 1 partialUpdate

record :: Uid Int Dat
record =
  Uid 1 (Dat 9 5 ["hello"])

target :: Uid Int Dat
target =
  Uid 1 (Dat 5 17.5 ["hello"])

test_partialUpdate :: UnitTest
test_partialUpdate =
  runTestAuto do
    target === (updatePartial partialUpdate <$> record)
