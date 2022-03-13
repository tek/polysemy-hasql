module Polysemy.Db.Text.Case where

import Data.Char (isLower, isUpper, toLower)
import Data.Composition ((.:))

unCamelCaseString :: Char -> String -> String
unCamelCaseString sep =
  fmap toLower . reverse . foldl f []
  where
    f [] c =
      [c]
    f (h1 : h2 : t) c | isLower c && isUpper h1 && isUpper h2 =
      c : h1 : sep : h2 : t
    f (h : t) c | isUpper c && isLower h =
      c : sep : h : t
    f z c =
      c : z

unCamelCase :: Char -> String -> Text
unCamelCase =
  toText .: unCamelCaseString

unCamelCaseText :: Char -> Text -> Text
unCamelCaseText sep =
  unCamelCase sep . toString
