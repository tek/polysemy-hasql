module Polysemy.Db.Text.Quote where

dquote :: Text -> Text
dquote a =
  [text|"#{a}"|]
