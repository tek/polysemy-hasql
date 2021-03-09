{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Query where

import Hasql.Encoders (Params)

import Polysemy.Hasql.Column.Class (TableColumn)
import qualified Polysemy.Hasql.Data.Query as Query
import Polysemy.Hasql.Data.Query (Query)
import Polysemy.Hasql.Data.Where (Where)
import Polysemy.Hasql.QueryParams (QueryParams, queryParams)
import Polysemy.Hasql.Table.QueryTable (GenQuery, genQuery)

interpretQueryWith ::
  Params q ->
  Where d q ->
  InterpreterFor (Query q d) r
interpretQueryWith params query =
  interpret \case
    Query.Params -> pure params
    Query.Query -> pure query

interpretQuery ::
  ∀ qrep rep q qc d r .
  TableColumn qrep q qc =>
  QueryParams qc q =>
  GenQuery qrep rep q d =>
  InterpreterFor (Query q d) r
interpretQuery =
  interpretQueryWith (queryParams @qc @q) (genQuery @qrep @rep @q @d)
