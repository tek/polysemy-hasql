module Polysemy.Db.Id where

import qualified Data.UUID as UUID

import Polysemy.Db.Data.Id (Id(..))
import Polysemy.Db.Random (Random, random, runRandomIO)

interpretIdUuid ::
  Member Random r =>
  InterpreterFor (Id UUID) r
interpretIdUuid =
  interpret \case
    Generate ->
      random

interpretIdUuidIO ::
  Member (Embed IO) r =>
  InterpreterFor (Id UUID) r
interpretIdUuidIO =
  runRandomIO . interpretIdUuid . raiseUnder

interpretIdUuidConst ::
  InterpreterFor (Id UUID) r
interpretIdUuidConst =
  interpret \case
    Generate ->
      pure UUID.nil

interpretIdState ::
  Members [State [i], Error Text] r =>
  InterpreterFor (Id i) r
interpretIdState =
  interpret \case
    Generate ->
      get >>= \case
        u : rest ->
          u <$ put rest
        [] ->
          throw "Id pool exhausted"
{-# INLINE interpretIdState #-}

interpretIdList ::
  Member (Error Text) r =>
  [i] ->
  InterpreterFor (Id i) r
interpretIdList pool =
  evalState pool . interpretIdState . raiseUnder
{-# INLINE interpretIdList #-}

interpretIdConst ::
  i ->
  InterpreterFor (Id i) r
interpretIdConst id' =
  interpret \case Generate -> pure id'
