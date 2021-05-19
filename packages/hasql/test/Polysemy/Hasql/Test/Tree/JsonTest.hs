module Polysemy.Hasql.Test.Tree.JsonTest where

import qualified Data.Aeson as Aeson
import Polysemy.Db.Tree.Partial (field, partial, (+>))
import Polysemy.Test (UnitTest, assertRight, runTestAuto, (===))

data Dat =
  Dat1 {
    int :: Int,
    double :: Double,
    txt :: Text
  }
  |
  Dat2 {
    int :: Int,
    nouble :: Double,
    rext :: Int
  }
  deriving (Eq, Show, Generic)

test_treeJson :: UnitTest
test_treeJson =
  runTestAuto do
    [text|{"txt":"update","int":10}|] === encoded
    assertRight tree (Aeson.eitherDecode encoded)
  where
    encoded =
      Aeson.encode tree
    tree =
      partial @Dat +> field @"int" (10 :: Int) +> field @"txt" ("update" :: Text)
