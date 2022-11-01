module Sqel.Data.Sql where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import Exon (
  ExonAppend (exonAppend, exonConcat),
  ExonExpression (exonExpression),
  Result (Empty, Result),
  SkipWs (SkipWs),
  ToSegment (toSegment),
  exonWith,
  skipWs,
  )
import Language.Haskell.TH.Quote (QuasiQuoter)

import Sqel.Text.Quote (dquote)

newtype Sql = Sql { unSql :: Text }
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (IsString, Semigroup, Monoid)

instance ConvertUtf8 Text bs => ConvertUtf8 Sql bs where
  encodeUtf8 = encodeUtf8 . unSql

  decodeUtf8 = Sql . decodeUtf8

  decodeUtf8Strict = fmap Sql . decodeUtf8Strict

sql :: QuasiQuoter
sql = exonWith (Just ([e|SkipWs|], [e|skipWs|])) True False

class ToSql a where
  toSql :: a -> Sql

instance ToSql Sql where
  toSql = id

instance {-# incoherent #-} ToSql a => ToSegment a Sql where
  toSegment = toSql

instance ExonExpression (SkipWs Sql) Text builder where
  exonExpression builder expr
    | Text.null expr = Empty
    | otherwise = Result (builder expr)

instance ExonAppend (SkipWs Sql) Text.Builder where
  exonConcat (h :| t) =
    go h t
    where
      go Empty (seg : segs) = go seg segs
      go z (Empty : Empty : segs) = go z (Empty : segs)
      go z [Empty] = z
      go z (Empty : segs) = go z (Result " " : segs)
      go (Result z) (Result seg : segs) = go (exonAppend @Sql z seg) segs
      go z [] = z

sqlQuote :: Text -> Sql
sqlQuote = Sql . dquote
