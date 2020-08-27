module Polysemy.Db.Test.Assert (
  module Polysemy.Db.Test.Assert,
  assert,
  evalEither,
  (===),
) where

import Hedgehog (TestT, assert, evalEither, (===))

assertRight ::
  Eq a =>
  Show a =>
  Show e =>
  Monad m =>
  a ->
  Either e a ->
  TestT m ()
assertRight target =
  (target ===) <=< evalEither

data ValueIsNothing =
  ValueIsNothing
  deriving Show

assertJust ::
  Eq a =>
  Show a =>
  Monad m =>
  a ->
  Maybe a ->
  TestT m ()
assertJust target =
  assertRight target . maybeToRight ValueIsNothing

evalMaybe ::
  Monad m =>
  Maybe a ->
  TestT m a
evalMaybe =
  evalEither . maybeToRight ValueIsNothing
