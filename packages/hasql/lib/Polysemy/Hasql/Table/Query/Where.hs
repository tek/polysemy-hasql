module Polysemy.Hasql.Table.Query.Where where

import Data.Symbol.Ascii (ToList)
import qualified Data.Text as Text
import Fcf (Eval, Exp, ZipWith)
import qualified Fcf.Data.Text as Fcf
import Fcf.Data.Text (FromList, ToSymbol, Uncons)
import Generics.SOP.GGP (GCode, GDatatypeInfoOf)
import Generics.SOP.Type.Metadata (ConstructorInfo(Record), DatatypeInfo(Newtype, ADT), FieldInfo(FieldInfo))
import Type.Errors (TypeError)
import Type.Errors.Pretty (type (%), type (<>))

import Fcf.Class.Functor (FMap)
import Polysemy.Db.Data.Column (Flatten, Sum)
import Polysemy.Db.Data.Cond (Greater, GreaterOrEq, Less, LessOrEq)
import Polysemy.Db.SOP.Constraint (IsData, slugString_, slugSymbol_, symbolString)
import Polysemy.Db.SOP.Error (ErrorWithType, ErrorWithType2, JoinComma)
import qualified Polysemy.Hasql.Data.QueryWhere as Data (QueryWhere(QueryWhere))
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import Polysemy.Hasql.Table.Query.Prepared (dollar)
import Polysemy.Hasql.Table.Representation (ProdColumn, ReifyRepTable, SumColumn)

simpleSlug ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  String
simpleSlug =
  [qt|"#{slugString_ (dropWhile ('_' ==) (symbolString @name))}"|]

data FieldName =
  FieldName Symbol

data ColumnName =
  SumName Symbol Symbol Symbol
  |
  SimpleName Symbol

class ReifyName (name :: ColumnName) where
  reifyName :: String

instance (
    KnownSymbol typeName,
    KnownSymbol ctorName,
    KnownSymbol name
  ) => ReifyName ('SumName typeName ctorName name) where
  reifyName =
    [qt|(#{simpleSlug @typeName}).#{simpleSlug @ctorName}.#{simpleSlug @name}|]

instance (
    KnownSymbol name
  ) => ReifyName ('SimpleName name) where
  reifyName =
    simpleSlug @name

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

concatWhereFields ::
  [Int -> Text] ->
  Text
concatWhereFields fields =
  Text.intercalate " and " (zipWith ($) fields [(1 :: Int)..length fields])

trueField :: Int -> Text
trueField _ =
  "true"

where' ::
  [Int -> Text] ->
  Data.QueryWhere a query
where' fields =
  Data.QueryWhere (SqlCode (concatWhereFields fields))

class QueryWhereColumn d q where
  queryWhereColumn :: String -> Int -> Text

instance {-# overlappable #-} (
  ErrorWithType2 "cannot query a column of type" d "with a query type" q
  ) => QueryWhereColumn d q where
  queryWhereColumn _ _ =
    "error"

instance {-# overlappable #-} QueryWhereColumn d d where
  queryWhereColumn =
    regularField

instance QueryWhereColumn d q => QueryWhereColumn d (Maybe q) where
  queryWhereColumn name i =
    maybeField i (queryWhereColumn @d @q name i)

instance QueryWhereColumn d q => QueryWhereColumn (Maybe d) q where
  queryWhereColumn =
    queryWhereColumn @d @q

instance QueryWhereColumn d (Less q) where
  queryWhereColumn =
    fieldWithOp "<"

instance QueryWhereColumn d (LessOrEq q) where
  queryWhereColumn =
    fieldWithOp "<="

instance QueryWhereColumn d (Greater q) where
  queryWhereColumn =
    fieldWithOp ">"

instance QueryWhereColumn d (GreaterOrEq q) where
  queryWhereColumn =
    fieldWithOp ">="

data PrimField =
  PrimField {
    rep :: *,
    tpe :: *,
    name :: ColumnName
  }
  |
  TrueField

data TableField =
  TableField {
    rep :: *,
    field :: Field
  }

type QFields =
  [PrimField]

data Field =
  Field * FieldName

type Fields =
  [Field]

data Con =
  Con Symbol Fields

type Cons =
  [Con]

data MkField :: * -> FieldName -> Exp Field

type instance (Eval (MkField d name)) =
  'Field d name

type family WithoutUnderscore (und :: Symbol) (cons :: Maybe (Symbol, Fcf.Text)) (name :: Symbol) :: Symbol where
  WithoutUnderscore und ('Just '(und, tail)) _ = Eval (ToSymbol tail)
  WithoutUnderscore _ _ name = name

data CanonicalName :: FieldInfo -> Exp FieldName

type instance (Eval (CanonicalName ('FieldInfo name))) =
  'FieldName (WithoutUnderscore "_" (Eval (Uncons (Eval (FromList (ToList name))))) name)

type family WithInfoData (dss :: [[*]]) (info :: DatatypeInfo) :: Fields where
  WithInfoData '[ds] ('Newtype _ _ ('Record _ fs)) =
    Eval (ZipWith MkField ds (Eval (FMap CanonicalName fs)))
  WithInfoData '[ds] ('ADT _ _ '[('Record _ fs)] _) =
    Eval (ZipWith MkField ds (Eval (FMap CanonicalName fs)))

type WithInfo a =
  WithInfoData (GCode a) (GDatatypeInfoOf a)

type family WithInfo2ADT (dss :: [[*]]) (info :: [ConstructorInfo]) :: Cons where
  WithInfo2ADT '[] '[] = '[]
  WithInfo2ADT (ds : dss) ('Record ctorName fs : fss) =
    'Con ctorName (Eval (ZipWith MkField ds (Eval (FMap CanonicalName fs)))) : WithInfo2ADT dss fss

type family WithInfo2Data (dss :: [[*]]) (info :: DatatypeInfo) :: Cons where
  WithInfo2Data dss ('ADT _ _ fss _) = WithInfo2ADT dss fss

type WithInfo2 a =
  WithInfo2Data (GCode a) (GDatatypeInfoOf a)

class QueryWhereFields' (fields :: QFields) where
  queryWhereFields' :: [Int -> Text]

instance QueryWhereFields' '[] where
  queryWhereFields' =
    mempty

instance (
    ReifyName name,
    QueryWhereColumn d q,
    QueryWhereFields' fields
  ) => QueryWhereFields' ('PrimField d q name : fields) where
  queryWhereFields' =
    queryWhereColumn @d @q (reifyName @name) : queryWhereFields' @fields

instance (
    QueryWhereFields' fields
  ) => QueryWhereFields' ('TrueField : fields) where
  queryWhereFields' =
    trueField : queryWhereFields' @fields

data Todo

data Table =
  Table {
    rowType :: *,
    fields :: [TableField],
    fieldNames :: [Symbol]
  }

data QueryTable =
  QueryTable {
    queryName :: Symbol,
    table :: Table
  }

type family FieldNames (fs :: [TableField]) :: [Symbol] where
  FieldNames '[] = '[]
  FieldNames ('TableField _ ('Field _ ('FieldName name)) : fs) = name : FieldNames fs

type family TableInfo (d :: *) (queryName :: Symbol) (ds :: [TableField]) :: QueryTable where
  TableInfo d queryName ds = 'QueryTable queryName ('Table d ds (FieldNames ds))

type family TableFields (table :: QueryTable) :: [TableField] where
  TableFields ('QueryTable _ ('Table _ fields _)) = fields

type family FlatProd (rep :: [*]) (ds :: Fields) :: [TableField] where
  FlatProd '[] '[] = '[]
  FlatProd (Flatten (ProdColumn rep) : reps) ('Field d _ : ds) =
    FlatProd (rep ++ reps) (WithInfo d ++ ds)
  FlatProd (rep : reps) (d : ds) =
    'TableField rep d : FlatProd reps ds
  FlatProd rep ds =
    ErrorWithType "FlatProd" '(rep, ds)

type family Flat (rep :: *) (d :: *) (queryName :: Symbol) (ds :: Fields) :: QueryTable where
  Flat _ d queryName '[] = TableInfo d queryName '[]
  Flat (ProdColumn rep) d queryName ds = TableInfo d queryName (FlatProd rep ds)
  Flat rep _ _ ds =
    ErrorWithType "Flat" '(rep, ds)

type family MatchCtor (rep :: *) (typeName :: Symbol) (ctorName :: Symbol) (d :: Fields) (q :: Fields) :: QFields where
  MatchCtor _ _ _ '[] '[] = '[]
  MatchCtor rep typeName ctorName ('Field d ('FieldName name) : ds) ('Field q ('FieldName name) : qs) =
    MatchField Todo d (Maybe q) ('SumName typeName ctorName name) ++ MatchCtor rep typeName ctorName ds qs
  MatchCtor rep _ _ _ _ =
    ErrorWithType "MatchCtor" rep

type family MatchSum (reps :: [*]) (typeName :: Symbol) (dss :: Cons) (qss :: Cons) :: QFields where
  MatchSum _ _ '[] '[] = '[]
  MatchSum (rep : reps) typeName ('Con ctorName d : dss) ('Con _ q : qss) =
    MatchCtor rep typeName ctorName d q ++ MatchSum reps typeName dss qss
  MatchSum reps _ _ _ =
    ErrorWithType "MatchSum" reps

type family MatchField (rep :: *) (d :: *) (q :: *) (name :: ColumnName) :: QFields where
  MatchField (Sum _ (SumColumn rep)) d q ('SimpleName n) =
    'TrueField : MatchSum rep n (WithInfo2 d) (WithInfo2 q)
  MatchField _ d q n = '[ 'PrimField d q n]

type family MissingField (table :: QueryTable) (q :: Field) :: k where
  MissingField ('QueryTable queryName ('Table _ _ fieldNames)) ('Field _ ('FieldName name)) =
    TypeError (
      "Unmatched column `" <> name <> "' in query type `" <> queryName <> "'" %
      "The database type has these columns:" %
      JoinComma fieldNames
    )

type family MatchQueryField (table :: QueryTable) (fields :: [TableField]) (q :: Field) :: QFields where
  MatchQueryField table '[] q =
    MissingField table q
  MatchQueryField _ ('TableField (Sum _ (SumColumn rep)) ('Field d ('FieldName n)) : _) ('Field q ('FieldName n)) =
    'TrueField : MatchSum rep n (WithInfo2 d) (WithInfo2 q)
  MatchQueryField _ ('TableField _ ('Field d ('FieldName n)) : _) ('Field q ('FieldName n)) =
    '[ 'PrimField d q ('SimpleName n)]
  MatchQueryField table (_ : tfs) q =
    MatchQueryField table tfs q

type family MatchQueryFields (table :: QueryTable) (qs :: Fields) :: QFields where
  MatchQueryFields _ '[] = '[]
  MatchQueryFields table (q : qs) = MatchQueryField table (TableFields table) q ++ MatchQueryFields table qs
  MatchQueryFields table qs =
    ErrorWithType "MatchQueryFields" '(table, qs)

-- Construct a @where@ fragment from two types, validating that all fields of the query record and their types are
-- present and matching in the data record
class QueryWhere (rep :: *) (d :: *) (query :: *) where
  queryWhere :: Data.QueryWhere d query

instance (
    IsData query (ts :: [*]) name,
    fields ~ MatchQueryFields (Flat (ReifyRepTable rep d) d name (WithInfo d)) (WithInfo query),
    QueryWhereFields' fields
  ) =>
  QueryWhere rep d query where
    queryWhere =
      where' (queryWhereFields' @fields)
