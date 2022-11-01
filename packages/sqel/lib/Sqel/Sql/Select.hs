module Sqel.Sql.Select where

import qualified Exon

import Sqel.Data.Dd (QOp)
import Sqel.Data.Selector (Selector)
import Sqel.Data.Sql (Sql (Sql), ToSql (toSql), sql)

data FragType =
  Where
  |
  Offset
  |
  Limit
  |
  Order
  |
  Custom Int Text
  deriving stock (Eq, Show, Generic)

fragKeyword :: FragType -> Sql
fragKeyword = \case
  Where -> "where"
  Offset -> "offset"
  Limit -> "limit"
  Order -> "order"
  Custom _ kw -> Sql kw

sfragOrdinal :: FragType -> Int
sfragOrdinal = \case
  Where -> 0
  Order -> 1
  Limit -> 2
  Offset -> 3
  Custom i _ -> i

instance Ord FragType where
  compare =
    comparing sfragOrdinal

data SelectAtom =
  SelectAtom {
    type_ :: FragType,
    code :: Selector -> Int -> Sql
  }
  deriving stock (Generic)

data SelectFragment =
  SelectFragment {
     type_ :: FragType,
     content :: Sql
  }
  deriving stock (Show, Eq, Generic, Ord)

data SelectExpr =
  SelectExprAtom FragType (Int -> Sql)
  |
  SelectExprList QOp [SelectExpr]
  |
  SelectExprSum [SelectExpr]
  |
  SelectExprNot SelectExpr

renderSelectFragment :: SelectFragment -> Sql
renderSelectFragment SelectFragment {..} =
  [sql|#{fragKeyword type_} #{content}|]

instance ToSql [SelectFragment] where
  toSql = Exon.intercalate " " . fmap renderSelectFragment . sort

newtype Select a =
  Select { unSelect :: a }
  deriving stock (Eq, Show)
