module Sqel.Query where

import qualified Data.Map.Strict as Map
import Prelude hiding (type (@@))

import Sqel.Data.Codec (Encoder)
import Sqel.Data.Dd (Dd, DdK, DdType)
import Sqel.Data.FragType (FragType)
import Sqel.Data.QuerySchema (QuerySchema (QuerySchema))
import Sqel.Data.Select (
  SelectExpr (SelectExprAtom, SelectExprIgnore, SelectExprList, SelectExprNot, SelectExprSum),
  SelectFragment (SelectFragment),
  )
import Sqel.Data.Sql (Sql)
import Sqel.Query.Check (CheckQueryFields)
import Sqel.Query.Fragments (ColumnPrefix (NoPrefix), joinFrag, joinSum)
import Sqel.Query.SelectExpr (ToSelectExpr (toSelectExpr))
import Sqel.ReifyCodec (ReifyCodec (reifyCodec))

class CheckedQ' (check :: Maybe Constraint) (s :: DdK) where
  checkedQ' :: Dd s -> SelectExpr

instance ToSelectExpr s => CheckedQ' 'Nothing s where
  checkedQ' = toSelectExpr NoPrefix

class CheckedQ (qs :: DdK) (ts :: DdK) where
  checkedQ :: Dd qs -> SelectExpr

instance (
    check ~ CheckQueryFields qs ts,
    CheckedQ' check qs
  ) => CheckedQ qs ts where
    checkedQ = checkedQ' @check

compileSelectExpr ::
  SelectExpr ->
  [SelectFragment]
compileSelectExpr expr =
  Map.elems (Map.mapWithKey SelectFragment (snd (spin 1 expr)))
  where
    spin :: Int -> SelectExpr -> (Int, Map FragType Sql)
    spin i = \case
      SelectExprAtom tpe code -> (i + 1, [(tpe, code i)])
      SelectExprList op sub -> second (Map.mapWithKey (joinFrag op)) (prod i sub)
      SelectExprSum sub -> second (Map.mapWithKey (joinSum i)) (prod (i + 1) sub)
      SelectExprNot _ -> undefined
      SelectExprIgnore -> (i, mempty)
    prod i sub =
      second (Map.unionsWith (<>) . fmap (fmap pure)) (mapAccumL spin i sub)

querySchemaWith ::
  ∀ q query a .
  ReifyCodec Encoder query q =>
  Dd query ->
  SelectExpr ->
  QuerySchema q a
querySchemaWith query expr =
  QuerySchema (compileSelectExpr expr) (reifyCodec @Encoder query)

unsafeQuerySchema ::
  ∀ q query a .
  ToSelectExpr query =>
  ReifyCodec Encoder query q =>
  Dd query ->
  QuerySchema q a
unsafeQuerySchema query =
  querySchemaWith query (toSelectExpr NoPrefix query)

class CheckQuery query table where
  checkQuery ::
    Dd query ->
    Dd table ->
    QuerySchema (DdType query) (DdType table)

type CheckQuery :: DdK -> DdK -> Constraint
instance (
    CheckedQ query table,
    ReifyCodec Encoder query (DdType query)
  ) => CheckQuery query table where
  checkQuery query _ =
    querySchemaWith query (checkedQ @query @table query)
