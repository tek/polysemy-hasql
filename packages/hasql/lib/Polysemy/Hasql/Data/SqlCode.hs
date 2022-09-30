module Polysemy.Hasql.Data.SqlCode where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text
import Exon (
  ExonAppend (exonAppend, exonConcat),
  ExonExpression (exonExpression),
  Result (Empty, Result),
  SkipWs (SkipWs),
  exonWith,
  skipWs,
  )
import Language.Haskell.TH.Quote (QuasiQuoter)

newtype SqlCode =
  SqlCode { unSqlCode :: Text }
  deriving stock (Show, Eq, Generic, Ord)
  deriving newtype (IsString, Semigroup, Monoid)

instance ConvertUtf8 Text bs => ConvertUtf8 SqlCode bs where
  encodeUtf8 =
    encodeUtf8 . unSqlCode

  decodeUtf8 =
    SqlCode . decodeUtf8

  decodeUtf8Strict =
    fmap SqlCode . decodeUtf8Strict

esql :: QuasiQuoter
esql =
  exonWith (Just ([e|SkipWs|], [e|skipWs|])) True False

instance ExonExpression (SkipWs SqlCode) Text builder where
  exonExpression builder expr
    | Text.null expr = Empty
    | otherwise = Result (builder expr)

instance ExonAppend (SkipWs SqlCode) Text.Builder where
  exonAppend z a =
    Result (z <> a)

  exonConcat (h :| t) =
    go h t
    where
      go Empty (seg : segs) = go seg segs
      go z (Empty : Empty : segs) = go z (Empty : segs)
      go z [Empty] = z
      go z (Empty : segs) = go z (Result " " : segs)
      go (Result z) (Result seg : segs) = go (exonAppend @SqlCode z seg) segs
      go z [] = z
