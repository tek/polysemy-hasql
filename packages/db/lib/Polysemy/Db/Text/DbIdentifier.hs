module Polysemy.Db.Text.DbIdentifier where

import Polysemy.Db.SOP.Constraint (symbolString, DataName (dataNameString))
import Polysemy.Db.Text.Case (unCamelCase)
import Polysemy.Db.Text.Quote (dquote)

dbIdentifier :: String -> Text
dbIdentifier =
  unCamelCase '_' . dropWhile ('_' ==)

dbIdentifierT :: Text -> Text
dbIdentifierT =
  dbIdentifier . toString

quotedDbId :: Text -> Text
quotedDbId =
  dquote . dbIdentifierT

dbSymbol ::
  ∀ name .
  KnownSymbol name =>
  Text
dbSymbol =
  dbIdentifier (symbolString @name)

dbSymbolBS ::
  ∀ name .
  KnownSymbol name =>
  ByteString
dbSymbolBS =
  encodeUtf8 (dbSymbol @name)

dbDataName ::
  ∀ d name .
  DataName d name =>
  Text
dbDataName =
  dbIdentifier (dataNameString @d)
