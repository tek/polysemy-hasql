module Polysemy.Hasql.Table.Query.Where where

import qualified Data.Text as Text
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo(Record), DatatypeInfo(Newtype, ADT), FieldInfo(FieldInfo))

import Fcf (Eval, Zip)
import Polysemy.Db.Data.Column (Flatten, Sum)
import Polysemy.Db.Data.Cond (Greater, GreaterOrEq, Less, LessOrEq)
import Polysemy.Db.SOP.Constraint (slugSymbolString_, slugSymbol_)
import qualified Polysemy.Hasql.Data.QueryWhere as Data (QueryWhere(QueryWhere))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Prepared (dollar)
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, SumColumn)

simpleSlug ::
  ∀ name .
  KnownSymbol name =>
  String
simpleSlug =
  [qt|"#{slugSymbolString_ @name}"|]

fieldWithOp ::
  Text ->
  String ->
  Int ->
  Text
fieldWithOp op name index =
  [qt|#{name} #{op} #{dollar index}|]

regularField ::
  String ->
  Int ->
  Text
regularField name =
  fieldWithOp "=" name

maybeField ::
  Int ->
  Text ->
  Text
maybeField index cond =
  [qt|(#{dollar index} is null or #{cond})|]

sumName ::
  ∀ (typeName :: Symbol) (ctorName :: Symbol) (name :: Symbol) .
  KnownSymbol typeName =>
  KnownSymbol ctorName =>
  KnownSymbol name =>
  String
sumName =
  [qt|("#{slugSymbol_ @typeName}")."#{slugSymbol_ @ctorName}"."#{slugSymbol_ @name}"|]

dummyField ::
  Int ->
  Text
dummyField _ =
  "true"

concatWhereFields ::
  [Int -> Text] ->
  Text
concatWhereFields fields =
  Text.intercalate " and " (zipWith ($) fields [(1 :: Int)..length fields])

where' ::
  [Int -> Text] ->
  Data.QueryWhere a query
where' fields =
  Data.QueryWhere (SqlCode (concatWhereFields fields))

class QueryWhereField d q where
  queryWhereField :: String -> Int -> Text

instance {-# overlappable #-} QueryWhereField d d where
  queryWhereField =
    regularField

instance QueryWhereField d q => QueryWhereField d (Maybe q) where
  queryWhereField name i =
    maybeField i (queryWhereField @d @q name i)

instance QueryWhereField d q => QueryWhereField (Maybe d) q where
  queryWhereField =
    queryWhereField @d @q

instance QueryWhereField d (Less q) where
  queryWhereField =
    fieldWithOp "<"

instance QueryWhereField d (LessOrEq q) where
  queryWhereField =
    fieldWithOp "<="

instance QueryWhereField d (Greater q) where
  queryWhereField =
    fieldWithOp ">"

instance QueryWhereField d (GreaterOrEq q) where
  queryWhereField =
    fieldWithOp ">="

type QFields =
  [(*, *, Symbol)]

type Fields =
  [(*, FieldInfo)]

type family WithInfoData (dss :: [[*]]) (info :: DatatypeInfo) :: Fields where
  WithInfoData '[ds] ('Newtype _ _ ('Record _ fs)) = Eval (Zip ds fs)
  WithInfoData '[ds] ('ADT _ _ '[('Record _ fs)] _) = Eval (Zip ds fs)

type WithInfo a =
  WithInfoData (GCode a) (GDatatypeInfoOf a)

type family WithInfo2ADT (dss :: [[*]]) (info :: [ConstructorInfo]) :: [(Symbol, Fields)] where
  WithInfo2ADT '[] '[] = '[]
  WithInfo2ADT (ds : dss) ('Record ctorName fs : fss) = '(ctorName, Eval (Zip ds fs)) : WithInfo2ADT dss fss

type family WithInfo2Data (dss :: [[*]]) (info :: DatatypeInfo) :: [(Symbol, Fields)] where
  WithInfo2Data dss ('ADT _ _ fss _) = WithInfo2ADT dss fss

type WithInfo2 a =
  WithInfo2Data (GCode a) (GDatatypeInfoOf a)

class QueryWhereCtor (ds :: Fields) (qss :: Fields) (typeName :: Symbol) (ctorName :: Symbol) where
  queryWhereCtor :: [Int -> Text]

instance QueryWhereCtor '[] '[] typeName ctorName where
  queryWhereCtor =
    mempty

instance (
    KnownSymbol typeName,
    KnownSymbol ctorName,
    KnownSymbol name,
    QueryWhereField d (Maybe q),
    QueryWhereCtor ds qs typeName ctorName
  ) => QueryWhereCtor ('(d, 'FieldInfo name) : ds) ('(q, 'FieldInfo name) : qs) typeName ctorName where
  queryWhereCtor =
     queryWhereField @d @(Maybe q) (sumName @typeName @ctorName @name) : queryWhereCtor @ds @qs @typeName @ctorName

class QueryWhereSum (dss :: [(Symbol, Fields)]) (qss :: [(Symbol, Fields)]) (name :: Symbol) where
  queryWhereSum :: [Int -> Text]

instance QueryWhereSum '[] '[] name where
  queryWhereSum =
    mempty

instance (
    QueryWhereCtor ds qs typeName ctorName,
    QueryWhereSum dss qss typeName
  ) => QueryWhereSum ('(ctorName, ds) : dss) ('(qcn, qs) : qss) typeName where
    queryWhereSum =
      queryWhereCtor @ds @qs @typeName @ctorName <> queryWhereSum @dss @qss @typeName

class QueryWhereProd (reps :: [*]) (ds ::  Fields) (qs :: Fields) where
  queryWhereProd :: [Int -> Text]

instance QueryWhereProd reps '[] '[] where
  queryWhereProd =
    mempty

instance {-# overlappable #-} (
    QueryWhereProd reps ds qs
  ) => QueryWhereProd (rep : reps) (d : ds) qs where
  queryWhereProd =
    queryWhereProd @reps @ds @qs

instance {-# overlappable #-} (
    KnownSymbol name,
    QueryWhereField d q,
    QueryWhereProd reps ds qs
  ) => QueryWhereProd (rep : reps) ('(d, 'FieldInfo name) : ds) ('(q, 'FieldInfo name) : qs) where
  queryWhereProd =
    queryWhereField @d @q (simpleSlug @name) : queryWhereProd @reps @ds @qs

instance (
    QueryWhereSum (WithInfo2 d) (WithInfo2 q) name,
    QueryWhereProd reps ds qs
  ) => QueryWhereProd (Sum (SumColumn rep) : reps) ('(d, 'FieldInfo name) : ds) ('(q, 'FieldInfo name) : qs) where
  queryWhereProd =
    dummyField : queryWhereSum @(WithInfo2 d) @(WithInfo2 q) @name <> queryWhereProd @reps @ds @qs

instance (
    QueryWhereFields' (ProdColumn (rep ++ reps)) (WithInfo d ++ ds) qs wit
  ) => QueryWhereProd (Flatten (ProdColumn rep) : reps) ('(d, 'FieldInfo name) : ds) qs where
  queryWhereProd =
    queryWhereFields' @(ProdColumn (rep ++ reps)) @(WithInfo d ++ ds) @qs @wit

class QueryWhereFields' (rep :: *) (ds ::  Fields) (qs :: Fields) (wit :: QFields) where
  queryWhereFields' :: [Int -> Text]

instance QueryWhereFields' rep '[] '[] wit where
  queryWhereFields' =
    mempty

instance (
    QueryWhereProd reps ds qs
  ) => QueryWhereFields' (ProdColumn reps) ds qs wit where
  queryWhereFields' =
    queryWhereProd @reps @ds @qs

type family MatchFields (ds :: Fields) (qs :: Fields) :: QFields where
  MatchFields ('(d, 'FieldInfo n) : ds) ('(q, 'FieldInfo n) : qs) = '(d, q, n) : MatchFields ds qs

-- Construct a @where@ fragment from two types, validating that all fields of the query record and their types are
-- present and matching in the data record
class QueryWhere' (rep :: *) (d :: *) (query :: *) where
  queryWhere' :: Data.QueryWhere d query

instance (
    QueryWhereFields' (ReifyRepTable rep d) (WithInfo d) (WithInfo query) (MatchFields (WithInfo d) (WithInfo query))
  ) =>
  QueryWhere' rep d query where
    queryWhere' =
      where' (queryWhereFields' @(ReifyRepTable rep d) @(WithInfo d) @(WithInfo query) @(MatchFields (WithInfo d) (WithInfo query)))
