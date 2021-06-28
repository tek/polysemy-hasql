module Polysemy.Hasql.Query where

import Hasql.Encoders (Params)
import Polysemy.Db.Data.Rep (Auto, UidRep)

import qualified Polysemy.Hasql.Data.Query as Query
import Polysemy.Hasql.Data.Query (Query, UidQuery)
import Polysemy.Hasql.Data.QueryTable (QueryTable (QueryTable))
import Polysemy.Hasql.Data.Where (Where)
import Polysemy.Hasql.Table.Schema (Schema, UidQuerySchema, schema)

interpretQueryWith ::
  Params q ->
  Where q d ->
  InterpreterFor (Query q d) r
interpretQueryWith params qwhere =
  interpret \case
    Query.Params -> pure params
    Query.Query -> pure qwhere
{-# inline interpretQueryWith #-}

interpretQuery ::
  ∀ qrep rep q d r .
  Schema qrep rep q d =>
  InterpreterFor (Query q d) r
interpretQuery =
  interpretQueryWith params qwhere
  where
    QueryTable _ params qwhere =
      schema @qrep @rep
{-# inline interpretQuery #-}

interpretQueryUid ::
  ∀ qrep irep rep i d r .
  UidQuerySchema qrep irep rep i i d =>
  InterpreterFor (UidQuery i d) r
interpretQueryUid =
  interpretQuery @qrep @(UidRep irep rep)
{-# inline interpretQueryUid #-}

interpretQueryAuto ::
  ∀ q d r .
  Schema Auto Auto q d =>
  InterpreterFor (Query q d) r
interpretQueryAuto =
  interpretQuery @Auto @Auto
{-# inline interpretQueryAuto #-}

interpretQuerySingleton ::
  InterpreterFor (Query q d) r
interpretQuerySingleton =
  interpret \case
    Query.Params -> pure mempty
    Query.Query -> pure mempty
{-# inline interpretQuerySingleton #-}
