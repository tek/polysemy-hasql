module Polysemy.Db.Text.Case where

import Data.Char (isLower, isUpper)
import qualified Data.Text as Text

unCamelCase :: Char -> String -> Text
unCamelCase sep =
  Text.toLower . toText . reverse . foldl f []
  where
    f [] c =
      [c]
    f (h1 : h2 : t) c | isLower c && isUpper h1 && isUpper h2 =
      c : h1 : sep : h2 : t
    f (h : t) c | isUpper c && isLower h =
      c : sep : h : t
    f z c =
      c : z

unCamelCaseText :: Char -> Text -> Text
unCamelCaseText sep =
  unCamelCase sep . toString
