{-# options_ghc -Wno-partial-type-signatures #-}

module Sqel.Test.TableSchemaTest where

import Hedgehog (TestT, (===))

import Sqel.Data.Dd (Dd, DdK (DdK), showTypeSel, (:>) ((:>)))
import Sqel.Names (typeAs)
import Sqel.Prim (prim)
import Sqel.Product2 (prod)
import Sqel.Data.TableSchema (TableSchema)
import Sqel.PgType (tableSchema)

data Thing =
  Thing {
    name :: Text,
    number :: Int64
  }
  deriving stock (Eq, Show, Generic)

dd :: Dd ('DdK _ _ Thing _)
dd =
  typeAs @"tab" (prod (prim :> prim))

schema :: TableSchema Thing
schema =
  tableSchema dd

test_tableSchema :: TestT IO ()
test_tableSchema = do
  "<type name for sqel_type__tab>" === showTypeSel dd
  "tab" === schema ^. #pg . #name
