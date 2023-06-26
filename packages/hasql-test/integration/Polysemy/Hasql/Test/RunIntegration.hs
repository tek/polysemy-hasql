module Polysemy.Hasql.Test.RunIntegration where

import Log (Severity (Debug))
import Polysemy.Test (UnitTest)

import Polysemy.Hasql.Test.Database (TestConnectionEffects)
import qualified Polysemy.Hasql.Test.Run as Lib
import Polysemy.Hasql.Test.Run (TestEffects)

integrationTestLevel ::
  HasCallStack =>
  Severity ->
  Sem (TestConnectionEffects ++ TestEffects) () ->
  UnitTest
integrationTestLevel level test =
  withFrozenCallStack do
    Lib.integrationTestLevel level "polysemy_db" test

integrationTestDebug ::
  HasCallStack =>
  Sem (TestConnectionEffects ++ TestEffects) () ->
  UnitTest
integrationTestDebug test =
  withFrozenCallStack do
    Lib.integrationTestLevel Debug "polysemy_db" test

integrationTest ::
  HasCallStack =>
  Sem (TestConnectionEffects ++ TestEffects) () ->
  UnitTest
integrationTest test =
  withFrozenCallStack do
    Lib.integrationTest "polysemy_db" test
