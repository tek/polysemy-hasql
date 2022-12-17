module Sqel.Query.Fragments where

import qualified Exon
import Exon (exon)

import Sqel.Data.Codec (ColumnName (ColumnName))
import Sqel.Data.Dd (QOp (QAnd, QOr))
import Sqel.Data.Sel (Sel (SelAuto, SelSymbol, SelUnused), SelW (SelWSymbol))
import Sqel.Data.Sql (Sql, sql)
import Sqel.Names (ddName)
import Sqel.Sql.Prepared (dollar)
import Sqel.Sql.Select (FragType (Where))
import Sqel.Text.DbIdentifier (quotedDbId)
import Data.Composition ((.:))

parens :: Sql -> Sql
parens s =
  [sql|(#{s})|]

joinOp :: QOp -> [Sql] -> Sql
joinOp =
  parens .: Exon.intercalate . sep
  where
    sep = \case
      QAnd -> " and "
      QOr -> " or "

joinFrag :: QOp -> FragType -> [Sql] -> Sql
joinFrag op = \case
  Where ->
    joinOp op
  _ ->
    fold . head

guardCon :: Int -> Int -> Sql -> Sql
guardCon index con code =
  [exon|(#{dollar index} = #{show con} and #{code})|]

joinSum :: Int -> FragType -> [Sql] -> Sql
joinSum index Where =
  joinOp QOr . fmap (uncurry (guardCon index)) . zip [0..]
joinSum _  _ =
  fold . head

data ColumnPrefix =
  NoPrefix
  |
  BasePrefix Text
  |
  TypePrefix Text
  deriving stock (Eq, Show)

-- TODO this quotes the segments that seem to work fine, while the ones in PgTable don't.
addPrefix ::
  ColumnName ->
  ColumnPrefix ->
  ColumnPrefix
addPrefix (ColumnName segment) = \case
  NoPrefix -> BasePrefix (quotedDbId segment)
  BasePrefix name -> TypePrefix [exon|(#{name}).#{quotedDbId segment}|]
  TypePrefix prefix -> TypePrefix [exon|#{prefix}.#{quotedDbId segment}|]

prefixed :: Text -> ColumnPrefix -> Text
prefixed name = \case
  NoPrefix -> quotedDbId name
  BasePrefix prefix -> [exon|(#{prefix}).#{quotedDbId name}|]
  TypePrefix prefix -> [exon|#{prefix}.#{quotedDbId name}|]

class QFragmentPrefix sel where
  qfragmentPrefix :: SelW sel -> ColumnPrefix -> ColumnPrefix

instance QFragmentPrefix ('SelSymbol n) where
  qfragmentPrefix (SelWSymbol _) = addPrefix (ddName @n)

instance QFragmentPrefix 'SelAuto where
  qfragmentPrefix _ = id

instance QFragmentPrefix 'SelUnused where
  qfragmentPrefix _ = id
