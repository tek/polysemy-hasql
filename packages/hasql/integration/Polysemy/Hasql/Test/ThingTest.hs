module Polysemy.Hasql.Test.ThingTest where

import Polysemy (run)
import Polysemy.Test (UnitTest)

data ThingKind =
  ThingKind Type

data Thing thing =
  Thing
  deriving Show

class MakeThing d (thing :: ThingKind) | d -> thing where
  makeThing :: d -> Thing thing

instance MakeThing d ('ThingKind d) where
  makeThing =
    undefined

data UseThing d :: Effect where
  UseThing :: Thing thing -> UseThing d m ()

makeSem ''UseThing

interpretUseThing :: InterpreterFor (UseThing d) r
interpretUseThing =
  interpret \ (UseThing t) ->
    dbgs t

data Dat =
  Dat { a :: Int }
  deriving (Eq, Show)

class ModThing (thing :: ThingKind) where
  modThing :: Thing thing -> Thing thing

instance ModThing thing where
  modThing =
    undefined

someThing ::
  MakeThing Dat thing =>
  Thing thing
someThing =
  makeThing (Dat 5)

prog :: Sem '[UseThing Dat] ()
prog =
  useThing someThing

test_thing :: UnitTest
test_thing =
  pure (run (interpretUseThing prog))
