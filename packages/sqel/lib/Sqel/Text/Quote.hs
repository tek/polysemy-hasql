module Sqel.Text.Quote where

import Exon (Exon, exon)

squote :: Exon a => a -> a
squote a =
  [exon|'#{a}'|]

dquote :: Exon a => a -> a
dquote a =
  [exon|"#{a}"|]
