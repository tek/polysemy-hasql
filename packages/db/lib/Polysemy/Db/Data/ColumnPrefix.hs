module Polysemy.Db.Data.ColumnPrefix where

import Exon (exon)

import Polysemy.Db.Text.DbIdentifier (quotedDbId)

data ColumnPrefix =
  InitPrefix
  |
  TablePrefix Text
  |
  BasePrefix Text
  |
  TypePrefix Text
  deriving stock (Eq, Show)

addPrefix ::
  Text ->
  ColumnPrefix ->
  ColumnPrefix
addPrefix segment = \case
  InitPrefix -> TablePrefix (quotedDbId segment)
  TablePrefix _ -> BasePrefix (quotedDbId segment)
  BasePrefix name -> TypePrefix [exon|(#{name}).#{quotedDbId segment}|]
  TypePrefix prefix -> TypePrefix [exon|#{prefix}.#{quotedDbId segment}|]

promotePrefix :: ColumnPrefix -> ColumnPrefix
promotePrefix = \case
  InitPrefix -> InitPrefix
  TablePrefix prefix -> BasePrefix prefix
  BasePrefix prefix -> BasePrefix prefix
  TypePrefix prefix -> TypePrefix prefix

prefixed :: Text -> ColumnPrefix -> Text
prefixed name = \case
  InitPrefix -> quotedDbId name
  TablePrefix _ -> quotedDbId name
  BasePrefix prefix -> [exon|(#{prefix}).#{quotedDbId name}|]
  TypePrefix prefix -> [exon|#{prefix}.#{quotedDbId name}|]
