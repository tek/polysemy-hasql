module Polysemy.Hasql.Test.Tree.JsonTest where

import qualified Data.Aeson as Aeson
import Polysemy.Db.Tree.Partial (field, partial, (+>))
import Polysemy.Test (UnitTest, assertRight, runTestAuto, (===))

data Sub =
  Sub {
    nouble :: Double
  }
  deriving (Eq, Show, Generic)

data Dat =
  Dat1 {
    int :: Int,
    double :: Double,
    txt :: Text
  }
  |
  Dat2 {
    int :: Int,
    sub :: Sub,
    rext :: Int
  }
  deriving (Eq, Show, Generic)

test_treeJson :: UnitTest
test_treeJson =
  runTestAuto do
    [text|{"Dat1":{"txt":"update","int":10},"Dat2":{"sub":{"nouble":9.2},"int":10}}|] === encoded
    assertRight tree (Aeson.eitherDecode encoded)
  where
    encoded =
      Aeson.encode tree
    tree =
      partial @Dat +> field @"int" (10 :: Int) +> field @"nouble" (9.2 :: Double) +> field @"txt" ("update" :: Text)
