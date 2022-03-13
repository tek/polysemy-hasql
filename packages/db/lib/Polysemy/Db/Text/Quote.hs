module Polysemy.Db.Text.Quote where

import Exon (exon)

dquote :: Text -> Text
dquote a =
  [exon|"#{a}"|]
