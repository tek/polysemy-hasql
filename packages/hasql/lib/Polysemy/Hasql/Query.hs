{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Query where

import Hasql.Encoders (Params)
import Polysemy.Db.Data.Column (Auto)

import qualified Polysemy.Hasql.Data.Query as Query
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.QueryTable (QueryTable(QueryTable))
import Polysemy.Hasql.Data.Where (Where)
import Polysemy.Hasql.Table.QueryTable (GenQueryTable, genQueryTable)

interpretQueryWith ::
  Params q ->
  Where d q ->
  InterpreterFor (Query q d) r
interpretQueryWith params qwhere =
  interpret \case
    Query.Params -> pure params
    Query.Query -> pure qwhere

interpretQuery ::
  ∀ qrep rep q d r .
  GenQueryTable qrep rep q d =>
  InterpreterFor (Query q d) r
interpretQuery =
  interpretQueryWith params qwhere
  where
    QueryTable _ params qwhere =
      genQueryTable @qrep @rep

interpretQueryAuto ::
  ∀ q d r .
  GenQueryTable Auto Auto q d =>
  InterpreterFor (Query q d) r
interpretQueryAuto =
  interpretQuery @Auto @Auto
