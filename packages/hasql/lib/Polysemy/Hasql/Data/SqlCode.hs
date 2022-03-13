module Polysemy.Hasql.Data.SqlCode where

import qualified Data.Text as Text
import Exon (Exon (..), ExonDefault, Result (Empty, Result))

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

instance Exon ExonDefault SqlCode where
  isEmpty =
    Text.null . unSqlCode

  insertWhitespace Empty _ s =
    convertSegment @ExonDefault s
  insertWhitespace (Result s1) _ s2 =
    Result (s1 <> " ") <> convertSegment @ExonDefault s2
