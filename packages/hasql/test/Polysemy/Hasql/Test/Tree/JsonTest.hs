module Polysemy.Hasql.Test.Tree.JsonTest where

import qualified Data.Aeson as Aeson
import Polysemy.Db.Tree.Data.PartialPayload (PartialPayload, partialPayload)
import Polysemy.Db.Tree.Partial (field, partially, (+>))
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

payload ::
  PartialPayload Dat
payload =
  partialPayload (partially @Dat +> field @"int" (10 :: Int) +> field @"nouble" (9.2 :: Double) +> field @"txt" ("update" :: Text))

test_treeJson :: UnitTest
test_treeJson =
  runTestAuto do
    target === encoded
    assertRight tree (Aeson.eitherDecode encoded)
    target === Aeson.encode payload
  where
    target =
      [text|{"Dat1":{"txt":"update","int":10},"Dat2":{"sub":{"nouble":9.2},"int":10}}|]
    encoded =
      Aeson.encode tree
    tree =
      partially @Dat +> field @"int" (10 :: Int) +> field @"nouble" (9.2 :: Double) +> field @"txt" ("update" :: Text)
