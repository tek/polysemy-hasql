module Polysemy.Hasql.Data.Where where

import Exon (exon)
import Hasql.DynamicStatements.Snippet (Snippet)
import qualified Text.Show as Show

import Polysemy.Hasql.Data.SqlCode (SqlCode)

data Where q d =
  Where {
    prepared :: SqlCode,
    fields :: q -> Snippet
  }

instance Semigroup (Where q d) where
  Where pl fl <> Where pr fr =
    Where (pl <> pr) (fl <> fr)

instance Monoid (Where q d) where
  mempty =
    Where mempty mempty

instance Show (Where q d) where
  show (Where p _) =
    [exon|Where #{show p}|]
