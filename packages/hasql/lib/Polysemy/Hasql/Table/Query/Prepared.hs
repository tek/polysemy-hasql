module Polysemy.Hasql.Table.Query.Prepared where

dollar :: Int -> Text
dollar i =
  [qt|$#{i}|]

assign :: Text -> Text -> Text
assign name value =
  [qt|#{name} = #{value}|]
