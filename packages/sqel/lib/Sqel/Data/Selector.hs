module Sqel.Data.Selector where

import Sqel.Data.Sql (Sql (Sql), ToSql (toSql), sql)
import Sqel.Text.Quote (dquote)

newtype Selector =
  Selector { unSelector :: Sql }
  deriving stock (Eq, Show, Generic, Ord)
  deriving newtype (IsString, Semigroup, Monoid)

textSelector :: Text -> Selector
textSelector =
  Selector . Sql

nameSelector :: Text -> Selector
nameSelector =
  textSelector . dquote

assign :: Selector -> Sql -> Sql
assign (Selector name) value =
  [sql|#{name} = #{value}|]

instance ToSql Selector where
  toSql = unSelector
