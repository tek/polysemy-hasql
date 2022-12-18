module Sqel.Data.Select where

import qualified Exon

import Sqel.Data.Dd (QOp)
import Sqel.Data.FragType (FragType, renderWithFragKeyword)
import Sqel.Data.Selector (Selector)
import Sqel.Data.Sql (Sql, ToSql (toSql))

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

renderSelectFragment :: SelectFragment -> Sql
renderSelectFragment SelectFragment {..} =
  renderWithFragKeyword content type_

instance ToSql [SelectFragment] where
  toSql = Exon.intercalate " " . fmap renderSelectFragment . sort

data SelectExpr =
  SelectExprAtom FragType (Int -> Sql)
  |
  SelectExprList QOp [SelectExpr]
  |
  SelectExprSum [SelectExpr]
  |
  SelectExprNot SelectExpr
  |
  SelectExprIgnore

newtype Select a =
  Select { unSelect :: a }
  deriving stock (Eq, Show)
