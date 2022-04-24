module Polysemy.Hasql.Test.Tree.JsonTest where

import qualified Data.Aeson as Aeson
import Exon (exon)
import Polysemy.Db.Data.Partial (partial)
import Polysemy.Db.Tree.Data.PartialPayload (
  PartialPayload,
  decodePartialPayload,
  decodePartialPayloadTree,
  partialPayload,
  )
import Polysemy.Db.Tree.Partial (Partial, PartialTree, field, partially, (++>), (+>))
import Polysemy.Db.Tree.Partial.Insert (InsertPaths, type (@>))
import Polysemy.Test (UnitTest, assertRight, runTestAuto, (===))

data Sub =
  Sub {
    nouble :: Double
  }
  deriving stock (Eq, Show, Generic)

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
  deriving stock (Eq, Show, Generic)

pTree ::
  InsertPaths Dat ["int" @> Int, "nouble" @> Double, "txt" @> Text] tree =>
  PartialTree tree
pTree =
  partially @Dat ++> field @"int" (10 :: Int) ++> field @"nouble" (9.2 :: Double) ++> field @"txt" ("update" :: Text)

parTree :: Partial Dat
parTree =
  partial @Dat +> field @"int" (10 :: Int) +> field @"nouble" (9.2 :: Double) +> field @"txt" ("update" :: Text)

payload ::
  PartialPayload Dat
payload =
  partialPayload (partially @Dat ++> field @"int" (10 :: Int) ++> field @"nouble" (9.2 :: Double) ++> field @"txt" ("update" :: Text))

test_treeJson :: UnitTest
test_treeJson =
  runTestAuto do
    target === encoded
    assertRight pTree (Aeson.eitherDecode encoded)
    target === encodedPayload
    assertRight pTree (decodePartialPayloadTree @Dat =<< first toText (Aeson.eitherDecode encodedPayload))
    assertRight parTree (decodePartialPayload @Dat =<< first toText (Aeson.eitherDecode encodedPayload))
  where
    target =
      [exon|{"Dat1":{"int":10,"txt":"update"},"Dat2":{"int":10,"sub":{"nouble":9.2}}}|]
    encoded =
      Aeson.encode pTree
    encodedPayload =
      Aeson.encode payload
