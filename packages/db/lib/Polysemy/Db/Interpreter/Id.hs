module Polysemy.Db.Interpreter.Id where

import Conc (interpretAtomic)
import qualified Data.UUID as UUID
import Data.UUID (UUID)

import Polysemy.Db.Effect.Id (Id (NewId))
import Polysemy.Db.Effect.Random (Random, random)
import Polysemy.Db.Interpreter.Random (interpretRandom)

interpretIdUuid ::
  Member (Random UUID) r =>
  InterpreterFor (Id UUID) r
interpretIdUuid =
  interpret \ NewId -> random

interpretIdUuidIO ::
  Member (Embed IO) r =>
  InterpreterFor (Id UUID) r
interpretIdUuidIO =
  interpretRandom . interpretIdUuid . raiseUnder

interpretIdUuidZero ::
  InterpreterFor (Id UUID) r
interpretIdUuidZero =
  interpret \ NewId -> pure UUID.nil

interpretIdAtomicState ::
  ∀ i r .
  Members [AtomicState [i], Error Text] r =>
  InterpreterFor (Id i) r
interpretIdAtomicState =
  interpret \case
    NewId -> do
      i <- atomicState' \case
        u : rest -> (rest, Just u)
        [] -> ([], Nothing)
      fromMaybeA (throw "Id pool exhausted") i

interpretIdList ::
  ∀ i r .
  Members [Error Text, Embed IO] r =>
  [i] ->
  InterpreterFor (Id i) r
interpretIdList pool =
  interpretAtomic pool .
  interpretIdAtomicState .
  raiseUnder

interpretIdNumFrom ::
  ∀ i r .
  Member (Embed IO) r =>
  Num i =>
  i ->
  InterpreterFor (Id i) r
interpretIdNumFrom start =
  interpretAtomic @i start .
  reinterpret \ NewId ->
    atomicState' \ i -> ((i + 1), i)

interpretIdNum ::
  ∀ i r .
  Member (Embed IO) r =>
  Num i =>
  InterpreterFor (Id i) r
interpretIdNum =
  interpretIdNumFrom 1

interpretIdNumLocal ::
  ∀ i r .
  Num i =>
  InterpreterFor (Id i) r
interpretIdNumLocal =
  evalState @i 1 .
  reinterpret \ NewId ->
    get >>= \ i -> i <$ put (i + 1)

interpretIdConst ::
  ∀ i r .
  i ->
  InterpreterFor (Id i) r
interpretIdConst i =
  interpret \ NewId -> pure i
