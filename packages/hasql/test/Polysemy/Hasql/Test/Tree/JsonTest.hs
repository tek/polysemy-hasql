module Polysemy.Hasql.Test.Tree.JsonTest where

import Polysemy.Test (UnitTest, assertRight, runTestAuto)
import qualified Data.Aeson as Aeson
import Polysemy.Db.Tree.Partial (partial, (+>), field)

data Dat =
  Dat {
    int :: Int,
    double :: Double,
    txt :: Text
  }
  deriving (Eq, Show, Generic)

test_treeJson :: UnitTest
test_treeJson =
  runTestAuto do
    assertRight tree (Aeson.eitherDecode (Aeson.encode tree))
  where
    tree =
      partial @Dat +> field @"int" (10 :: Int) +> field @"txt" ("update" :: Text)
