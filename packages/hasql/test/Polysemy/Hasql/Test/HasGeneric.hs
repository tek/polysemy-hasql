module Polysemy.Hasql.Test.HasGeneric where

import Generics.SOP.GGP (GCode)
import Polysemy.Db.SOP.HasGeneric (gcodeResolvesNot, hasNoGeneric)
import Polysemy.Test (UnitTest, runTestAuto, (===))

data Dat =
  Dat { int :: Int }
  deriving (Eq, Show, Generic)

test_hasGeneric :: UnitTest
test_hasGeneric =
  runTestAuto do
    True === gcodeResolvesNot @(GCode Int)
    False === gcodeResolvesNot @(GCode Dat)
    True === hasNoGeneric @Int
    False === hasNoGeneric @Dat
