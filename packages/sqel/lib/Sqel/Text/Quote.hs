module Sqel.Text.Quote where

import Exon (exon)

squote :: Text -> Text
squote a =
  [exon|'#{a}'|]

dquote :: Text -> Text
dquote a =
  [exon|"#{a}"|]
