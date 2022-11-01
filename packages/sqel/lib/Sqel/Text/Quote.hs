module Sqel.Text.Quote where

import Exon (exon)

dquote :: Text -> Text
dquote a =
  [exon|"#{a}"|]
