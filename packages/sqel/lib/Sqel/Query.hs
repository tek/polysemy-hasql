module Sqel.Query where

import qualified Data.Map.Strict as Map
import Prelude hiding (type (@@))

import Sqel.Data.Codec (Encoder)
import Sqel.Data.Dd (Dd, DdK, DdType)
import Sqel.Data.QuerySchema (QuerySchema (QuerySchema))
import Sqel.Data.Sql (Sql)
import Sqel.Query.Check (CheckQuery)
import Sqel.Query.Fragments (ColumnPrefix (NoPrefix), joinFrag, joinSum)
import Sqel.Query.SelectExpr (ToSelectExpr (toSelectExpr))
import Sqel.ReifyCodec (ReifyCodec (reifyCodec))
import Sqel.Sql.Select (
  FragType,
  SelectExpr (SelectExprAtom, SelectExprList, SelectExprNot, SelectExprSum),
  SelectFragment (SelectFragment),
  )

class CheckedQ' (check :: Maybe Constraint) (s :: DdK) where
  checkedQ' :: Dd s -> SelectExpr

instance ToSelectExpr s => CheckedQ' 'Nothing s where
  checkedQ' = toSelectExpr NoPrefix

-- instance err => CheckedQ' ('Just err) s where
--   checkedQ' _ =
--     error "unreachable"

class CheckedQ (qs :: DdK) (ts :: DdK) where
  checkedQ :: Dd qs -> SelectExpr

instance (
    check ~ CheckQuery qs ts,
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

unsafeQueryMeta ::
  ∀ q query a .
  ToSelectExpr query =>
  ReifyCodec Encoder query q =>
  Dd query ->
  QuerySchema q a
unsafeQueryMeta query =
  querySchemaWith query (toSelectExpr NoPrefix query)

checkQuery ::
  ∀ query table .
  CheckedQ query table =>
  ReifyCodec Encoder query (DdType query) =>
  Dd query ->
  Dd table ->
  QuerySchema (DdType query) (DdType table)
checkQuery query _ =
  querySchemaWith query (checkedQ @query @table query)
