module Polysemy.Hasql.Data.Where where

import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Text.Show as Show

import Polysemy.Hasql.Data.SqlCode (SqlCode)

data Where d q =
  Where {
    prepared :: SqlCode,
    fields :: q -> Snippet
  }

instance Semigroup (Where d q) where
  Where pl fl <> Where pr fr =
    Where (pl <> pr) (fl <> fr)

instance Monoid (Where d q) where
  mempty =
    Where mempty mempty

instance Show (Where d q) where
  show (Where p _) =
    [text|Where #{p}|]
