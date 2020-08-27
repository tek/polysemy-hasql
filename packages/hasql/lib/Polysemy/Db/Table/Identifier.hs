module Polysemy.Db.Table.Identifier where

import Polysemy.Db.Text.Case (unCamelCase)

dbIdentifier :: String -> Text
dbIdentifier =
  unCamelCase '_'
