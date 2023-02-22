module Polysemy.Db.Interpreter.Id where

import Conc (interpretAtomic)
import qualified Data.UUID as UUID
import Data.UUID (UUID)

import Polysemy.Db.Effect.Id (Id (NewId))
import Polysemy.Db.Random (Random, random, runRandomIO)

interpretIdUuid ::
  Member Random r =>
  InterpreterFor (Id UUID) r
interpretIdUuid =
  interpret \ NewId -> random

interpretIdUuidIO ::
  Member (Embed IO) r =>
  InterpreterFor (Id UUID) r
interpretIdUuidIO =
  runRandomIO . interpretIdUuid . raiseUnder

interpretIdUuidZero ::
  InterpreterFor (Id UUID) r
interpretIdUuidZero =
  interpret \ NewId -> pure UUID.nil

interpretIdState ::
  Members [AtomicState [i], Error Text] r =>
  InterpreterFor (Id i) r
interpretIdState =
  interpret \case
    NewId -> do
      i <- atomicState' \case
        u : rest -> (rest, Just u)
        [] -> ([], Nothing)
      fromMaybeA (throw "Id pool exhausted") i

interpretIdList ::
  Members [Error Text, Embed IO] r =>
  [i] ->
  InterpreterFor (Id i) r
interpretIdList pool =
  interpretAtomic pool .
  interpretIdState .
  raiseUnder

interpretIdNum ::
  ∀ i r .
  Member (Embed IO) r =>
  Num i =>
  InterpreterFor (Id i) r
interpretIdNum =
  interpretAtomic @i 1 .
  reinterpret \ NewId ->
    atomicState' \ id' -> ((id' + 1), id')

interpretIdConst ::
  i ->
  InterpreterFor (Id i) r
interpretIdConst id' =
  interpret \ NewId -> pure id'
