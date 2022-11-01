{-# options_ghc -Wno-partial-type-signatures #-}

module Polysemy.Hasql.Test.Dsl.TableSchemaTest where

import Polysemy.Test (UnitTest, runTestAuto, (===))
import Sqel.Data.Dd (Dd, DdK (DdK), showTypeSel, (:>) ((:>)))
import Sqel.Data.TableSchema (TableSchema)
import Sqel.Names (typeAs)
import Sqel.PgType (tableSchema)
import Sqel.Prim (prim)
import Sqel.Product (prod)

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

test_dslTableSchema :: UnitTest
test_dslTableSchema =
  runTestAuto do
    "tab" === showTypeSel dd
    "tab" === schema ^. #pg ^. #name
