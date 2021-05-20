module Polysemy.Hasql.Query where

import Hasql.Encoders (Params)
import Polysemy.Db.Data.Column (Auto)

import qualified Polysemy.Hasql.Data.Query as Query
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.Where (Where)
import Polysemy.Hasql.Table.Schema (Schema, schema)

interpretQueryWith ::
  Params q ->
  Where d q ->
  InterpreterFor (Query q d) r
interpretQueryWith params qwhere =
  interpret \case
    Query.Params -> pure params
    Query.Query -> pure qwhere
{-# INLINE interpretQueryWith #-}

interpretQuery ::
  ∀ qrep rep q d r .
  Schema qrep rep q d =>
  InterpreterFor (Query q d) r
interpretQuery =
  interpretQueryWith params qwhere
  where
    QueryTable _ params qwhere =
      schema @qrep @rep
{-# INLINE interpretQuery #-}

interpretQueryAuto ::
  ∀ q d r .
  Schema Auto Auto q d =>
  InterpreterFor (Query q d) r
interpretQueryAuto =
  interpretQuery @Auto @Auto
{-# INLINE interpretQueryAuto #-}

interpretQuerySingleton ::
  InterpreterFor (Query q d) r
interpretQuerySingleton =
  interpret \case
    Query.Params -> pure mempty
    Query.Query -> pure mempty
{-# INLINE interpretQuerySingleton #-}
