module Polysemy.Db.Interpreter.Id where

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
  Members [State [i], Error Text] r =>
  InterpreterFor (Id i) r
interpretIdState =
  interpret \case
    NewId ->
      get >>= \case
        u : rest -> u <$ put rest
        [] -> throw "Id pool exhausted"

interpretIdList ::
  Member (Error Text) r =>
  [i] ->
  InterpreterFor (Id i) r
interpretIdList pool =
  evalState pool .
  interpretIdState .
  raiseUnder

interpretIdNum ::
  ∀ i r .
  Num i =>
  InterpreterFor (Id i) r
interpretIdNum =
  evalState @i 1 .
  reinterpret \ NewId ->
    get >>= \ id' -> id' <$ put (id' + 1)

interpretIdConst ::
  i ->
  InterpreterFor (Id i) r
interpretIdConst id' =
  interpret \ NewId -> pure id'
