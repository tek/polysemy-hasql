module Sqel.Test.HasGeneric where

import Generics.SOP.GGP (GCode)
import Hedgehog (TestT, (===))

import Sqel.SOP.HasGeneric (gcodeResolvesNot, hasNoGeneric)

data Dat =
  Dat { int :: Int }
  deriving stock (Eq, Show, Generic)

test_hasGeneric :: TestT IO ()
test_hasGeneric = do
  True === gcodeResolvesNot @(GCode Int)
  False === gcodeResolvesNot @(GCode Dat)
  True === hasNoGeneric @Int
  False === hasNoGeneric @Dat
