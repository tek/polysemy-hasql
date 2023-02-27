module Polysemy.Db.Interpreter.Random where

import Conc (interpretAtomic)
import qualified System.Random as R

import Polysemy.Db.Effect.Random (Random (Random, RandomR))

interpretRandomAtomicState ::
  ∀ a q r .
  R.Random a =>
  R.RandomGen q =>
  Member (AtomicState q) r =>
  InterpreterFor (Random a) r
interpretRandomAtomicState =
  interpret \case
    Random ->
      atomicState' (swap . R.random)
    RandomR r ->
      atomicState' (swap . R.randomR r)

interpretRandomAtomic ::
  ∀ a q r .
  R.Random a =>
  R.RandomGen q =>
  Member (Embed IO) r =>
  q ->
  InterpreterFor (Random a) r
interpretRandomAtomic q =
  interpretAtomic q . interpretRandomAtomicState . raiseUnder

interpretRandomState ::
  ∀ a q r .
  R.Random a =>
  R.RandomGen q =>
  q ->
  InterpreterFor (Random a) r
interpretRandomState q =
  evalState q . atomicStateToState . interpretRandomAtomicState . raiseUnder2

interpretRandom ::
  ∀ a r .
  R.Random a =>
  Member (Embed IO) r =>
  InterpreterFor (Random a) r
interpretRandom sem = do
  q <- embed R.newStdGen
  interpretRandomAtomic q sem
