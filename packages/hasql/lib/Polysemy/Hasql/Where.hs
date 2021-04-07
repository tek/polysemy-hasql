module Polysemy.Hasql.Where where

import Data.Foldable (foldl1)
import qualified Data.Text as Text
import Fcf (Eval, Exp, FromMaybe, If, IsJust, Pure, Pure1, type (@@))
import Fcf.Class.Foldable (FoldMap)
import Fcf.Class.Functor (FMap)
import Generics.SOP (All, K(K), NP, hcollapse, hcpure)
import Polysemy.Db.Data.Cond (Greater, GreaterOrEq, Less, LessOrEq)
import Polysemy.Db.Data.FieldId (FieldId, FieldIdSymbol, FieldIdText, JoinCommaFieldIds, quotedFieldId)
import Polysemy.Db.SOP.Constraint (slugString_, symbolString)
import Polysemy.Db.SOP.Error (ErrorWithType, ErrorWithType2)
import Type.Errors (ErrorMessage(ShowType), TypeError)
import Type.Errors.Pretty (type (%), type (<>))

import Polysemy.Hasql.Column.Data.Effect (ContainsFlatten)
import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import qualified Polysemy.Hasql.Data.Where as Data (Where(Where))
import qualified Polysemy.Hasql.Kind.Data.DbType as Kind
import Polysemy.Hasql.Table.Query.Prepared (dollar)

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

fieldWithOp ::
  Text ->
  Text ->
  Int ->
  Text
fieldWithOp op name index =
  [qt|#{name} #{op} #{dollar index}|]

regularField ::
  Text ->
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
  Data.Where a query
where' fields =
  Data.Where (SqlCode (concatWhereFields fields))

class QueryWhereColumn q d where
  queryWhereColumn :: Text -> Int -> Text

instance {-# overlappable #-} (
  ErrorWithType2 "cannot query a column of type" d "with a query type" q
  ) => QueryWhereColumn q d where
  queryWhereColumn _ _ =
    "error"

instance {-# overlappable #-} QueryWhereColumn d d where
  queryWhereColumn =
    regularField

instance QueryWhereColumn q d => QueryWhereColumn (Maybe q) (Maybe d) where
  queryWhereColumn name i =
    maybeField i (queryWhereColumn @q @d name i)

instance {-# overlappable #-} QueryWhereColumn q d => QueryWhereColumn (Maybe q) d where
  queryWhereColumn name i =
    maybeField i (queryWhereColumn @q @d name i)

instance {-# overlappable #-} QueryWhereColumn q d => QueryWhereColumn q (Maybe d) where
  queryWhereColumn =
    queryWhereColumn @q @d

instance QueryWhereColumn (Less q) d where
  queryWhereColumn =
    fieldWithOp "<"

instance QueryWhereColumn (LessOrEq q) d where
  queryWhereColumn =
    fieldWithOp "<="

instance QueryWhereColumn (Greater q) d where
  queryWhereColumn =
    fieldWithOp ">"

instance QueryWhereColumn (GreaterOrEq q) d where
  queryWhereColumn =
    fieldWithOp ">="

data Field =
  Field * FieldName

type Fields =
  [Field]

data Segment =
  FieldSegment FieldId
  |
  SumSegment FieldId
  |
  ConSegment FieldId

class ReifySegment (s :: Segment) where
  reifySegment :: Text

instance FieldIdText id => ReifySegment ('SumSegment id) where
  reifySegment =
    quotedFieldId @id

instance FieldIdText id => ReifySegment ('ConSegment id) where
  reifySegment =
    quotedFieldId @id

instance FieldIdText id => ReifySegment ('FieldSegment id) where
  reifySegment =
    quotedFieldId @id

class ReifySegments (s :: [Segment]) where
  reifySegments :: [Text]

instance ReifySegments '[] where
  reifySegments =
    mempty

instance (
    ReifySegment s,
    ReifySegments ss
  ) => ReifySegments (s : ss) where
  reifySegments =
    reifySegment @s : reifySegments @ss

reifyPath ::
  ∀ s .
  ReifySegments s =>
  Text
reifyPath =
  case reverse (reifySegments @s) of
    [] -> ""
    [prim] -> prim
    base : rest -> [qt|(#{base}).#{Text.intercalate "." rest}|]

data QCond =
  SimpleCond {
    queryType :: *,
    dataType :: *,
    name :: [Segment]
  }
  |
  SumPrimCond {
    queryType :: *,
    dataType :: *,
    names :: [[Segment]]
  }
  |
  TrueCond

type family AsSimple (q :: *) (d :: *) (ns :: [[Segment]]) :: [QCond] where
  AsSimple q d ns =
    FMap (Pure1 ('SimpleCond q d)) @@ ns

type family WithoutMaybe (d :: *) :: * where
  WithoutMaybe (Maybe d) = d
  WithoutMaybe d = d

data TableField =
  TableField {
    rep :: *,
    field :: Field
  }

type QConds =
  [QCond]

class QueryCond (field :: QCond) where
  queryCond :: Int -> Text

instance (
    ReifySegments path,
    QueryWhereColumn q d
  ) => QueryCond ('SimpleCond q d path) where
  queryCond =
    queryWhereColumn @q @d (reifyPath @path)

instance (
    simple ~ AsSimple q (WithoutMaybe d) ns,
    All QueryCond simple
  ) => QueryCond ('SumPrimCond q d ns) where
  queryCond =
    fromMaybe (const "") (foldl1 (\ z a -> \ i -> (z i) <> " or " <> (a i)) <$> nonEmpty cs)
    where
      cs :: [Int -> Text]
      cs =
        hcollapse (hcpure (Proxy @QueryCond) f :: NP (K (Int -> Text)) simple)
      f :: ∀ c . QueryCond c => K (Int -> Text) c
      f =
        K (queryCond @c)

class QueryConds (fields :: QConds) where
  queryConds :: [Int -> Text]

instance QueryConds '[] where
  queryConds =
    mempty

instance (
    QueryCond field,
    QueryConds fields
  ) => QueryConds (field : fields) where
  queryConds =
    queryCond @field : queryConds @fields

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

type family MissingColumn (meta :: QueryMeta) (q :: Kind.Column) :: k where
  MissingColumn ('QueryMeta rep queryName fieldNames) ('Kind.Column name _ _) =
    TypeError ((
      "Unmatched column `" <> FieldIdSymbol @@ name <> "' in query type `" <> FieldIdSymbol @@ queryName <> "'" %
      "The database type has these columns:" %
      JoinCommaFieldIds fieldNames
    ) % 'ShowType rep)

data FirstJust :: (a -> Exp (Maybe b)) -> [a] -> Exp (Maybe b)
type instance Eval (FirstJust _ '[]) =
  'Nothing
type instance Eval (FirstJust p (a : as)) =
  If (IsJust @@ (p @@ a)) (Pure (p @@ a)) @@ (FirstJust p as)

type family ForceMaybe (d :: *) :: * where
  ForceMaybe (Maybe d) = Maybe d
  ForceMaybe d = Maybe d

type family ReplicateSum (qCol :: Kind.Column) (dCols :: [Kind.Column]) :: [Kind.Column] where
  ReplicateSum _ '[] =
    '[]
  ReplicateSum qCol (_ : dCols) =
    qCol : ReplicateSum qCol dCols

type family SumPrimNames (q :: *) (d :: *) (cs :: [QCond]) :: [[Segment]] where
  SumPrimNames _ _ '[] =
    '[]
  SumPrimNames q d ('SimpleCond q d n : cs) =
    n : SumPrimNames q d cs
  SumPrimNames q d ('SimpleCond q1 d1 n : _) =
    TypeError (
      "internal [SumPrimNames]:" %
      "type mismatch between column types of query and data for prim/sum query:" %
      "first constructor: " <> 'ShowType q <> " / " <> 'ShowType d %
      "offending constructor: " <> 'ShowType q1 <> " / " <> 'ShowType d1 %
      "path:" <> 'ShowType n
    )
  SumPrimNames _ _ cs =
    '[ '[ErrorWithType "internal SumPrimNames: cannot group non-simple conds" cs]]

type family GroupSumPrim (simple :: QConds) :: QConds where
  GroupSumPrim ('SimpleCond q d n : cs) =
    '[ 'SumPrimCond q d (n : SumPrimNames q d cs)]
  GroupSumPrim cs =
    '[ErrorWithType "internal GroupSumPrim: cannot group non-simple conds" cs]

type family MatchPrim (prefix :: [Segment]) (qname :: FieldId) (dname :: FieldId) (q :: *) (d :: *) :: Maybe QConds where
  MatchPrim (p : ps) name name q d =
    'Just '[ 'SimpleCond (ForceMaybe q) d (('FieldSegment name) : p : ps)]
  MatchPrim prefix name name q d =
    'Just '[ 'SimpleCond q d (('FieldSegment name) : prefix)]
  MatchPrim _ _ _ _ _ =
    'Nothing

type family MatchCon (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Column) (dCol :: Kind.Column) :: QConds where
  MatchCon meta prefix ('Kind.Column _ _ ('Kind.Prod _ q)) ('Kind.Column conName _ ('Kind.Prod _ d)) =
    FoldMap (MatchQueryColumnE meta ('ConSegment conName : prefix) d) @@ q
  MatchCon meta prefix ('Kind.Column qname _ ('Kind.Prim q)) ('Kind.Column conName _ ('Kind.Prod _ d)) =
    MatchQueryColumnE meta ('ConSegment conName : prefix) d @@ ('Kind.Column qname '[] ('Kind.Prim q))
  -- TODO
  MatchCon _ prefix ('Kind.Column qname _ ('Kind.Prim q)) ('Kind.Column qname _ ('Kind.Prim d)) =
    FromMaybe '[] @@ MatchPrim prefix qname qname q d
  MatchCon _ _ _ _ =
    '[]

type family MatchQCon (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Column) (dCols :: [Kind.Column]) :: QConds where
  MatchQCon _ _ _ '[] =
    '[]
  MatchQCon meta prefix qCol (dCol : dCols) =
    MatchCon meta prefix qCol dCol ++ MatchQCon meta prefix qCol dCols

type family MatchCons (meta :: QueryMeta) (prefix :: [Segment]) (qCols :: [Kind.Column]) (dCols :: [Kind.Column]) :: QConds where
  MatchCons _ _ '[] _ = '[]
  MatchCons meta prefix (qCol : qCols) (dCol : dCols) =
    MatchCon meta prefix qCol dCol ++ MatchCons meta prefix qCols dCols

type family MatchProd (meta :: QueryMeta) (prefix :: [Segment]) (flatten :: Bool) (qCol :: Kind.Column) (dCol :: Kind.Column) :: Maybe QConds where
  MatchProd meta prefix 'True q ('Kind.Column _ _ ('Kind.Prod  _ cols)) =
    MatchCols meta prefix q cols
  MatchProd meta prefix 'False ('Kind.Column qname eff ('Kind.Prim q)) ('Kind.Column name _ ('Kind.Prod _ cols)) =
    MatchCols meta ('FieldSegment name : prefix) ('Kind.Column qname eff ('Kind.Prim q)) cols
  MatchProd _ _ _ _ _ =
    'Nothing

type family MatchCol' (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Column) (dCol :: Kind.Column) :: Maybe QConds where
  MatchCol' _ prefix ('Kind.Column qname _ ('Kind.Prim q)) ('Kind.Column dname _ ('Kind.Prim d)) =
    MatchPrim prefix qname dname q d
  MatchCol' meta prefix q ('Kind.Column name effs ('Kind.Prod d cols)) =
    MatchProd meta prefix (ContainsFlatten effs) q ('Kind.Column name effs ('Kind.Prod d cols))
  MatchCol' meta prefix ('Kind.Column qname _ ('Kind.Sum _ qCols)) ('Kind.Column qname _ ('Kind.Sum _ dCols)) =
    'Just (MatchCons meta ('SumSegment qname : prefix) qCols dCols)
  MatchCol' _ _ ('Kind.Column qname _ ('Kind.Sum _ _)) ('Kind.Column qname _ _) =
    'Just (TypeError ("Query column " <> qname <> " is a sum type, but the data column is not."))
  MatchCol' _ _ _ _ =
    'Nothing

data MatchCol (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Column) :: Kind.Column -> Exp (Maybe QConds)
type instance Eval (MatchCol meta prefix qCol dCol) =
  MatchCol' meta prefix qCol dCol

type family MatchCols (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Column) (dCols :: [Kind.Column]) :: Maybe QConds where
  MatchCols meta prefix qCol dCols =
    FirstJust (MatchCol meta prefix qCol) @@ dCols

data MatchQueryColumnE (meta :: QueryMeta) (prefix :: [Segment]) :: [Kind.Column] -> Kind.Column -> Exp QConds
type instance Eval (MatchQueryColumnE meta prefix dCols qCol) =
  FromMaybe (MissingColumn meta qCol) @@ MatchCols meta prefix qCol dCols

data QueryMeta =
  QueryMeta {
    rep :: Kind.Column,
    query :: FieldId,
    fields :: [FieldId]
  }

data DbTypeFieldNames :: Kind.Column -> Exp [FieldId]

type instance Eval (DbTypeFieldNames ('Kind.Column name _ ('Kind.Prim _))) =
  '[name]
type instance Eval (DbTypeFieldNames ('Kind.Column _ _ ('Kind.Prod _ cols))) =
  FoldMap DbTypeFieldNames @@ cols
type instance Eval (DbTypeFieldNames ('Kind.Column name _ ('Kind.Sum _ cols))) =
  name : FoldMap DbTypeFieldNames @@ cols

type family MkQueryMeta (qCol :: Kind.Column) (dCol :: Kind.Column) :: QueryMeta where
  MkQueryMeta ('Kind.Column name _ _) dCol =
    'QueryMeta dCol name (DbTypeFieldNames @@ dCol)

type family MatchTable (meta :: QueryMeta) (qCol :: Kind.Column) (dCol :: Kind.Column) :: QConds where
  MatchTable meta ('Kind.Column _ _ ('Kind.Prod _ qCols)) ('Kind.Column _ _ ('Kind.Prod _ dCols)) =
    FoldMap (MatchQueryColumnE meta '[] dCols) @@ qCols
  MatchTable meta ('Kind.Column n e ('Kind.Prim t)) ('Kind.Column _ _ ('Kind.Prod _ dCols)) =
    MatchQueryColumnE meta '[] dCols @@ ('Kind.Column n e ('Kind.Prim t))
  MatchTable meta ('Kind.Column _ _ ('Kind.Sum _ qCols)) ('Kind.Column _ _ ('Kind.Sum _ dCols)) =
    MatchCons meta '[] qCols dCols
  MatchTable meta ('Kind.Column qn e ('Kind.Prim t)) ('Kind.Column _ _ ('Kind.Sum _ dCols)) =
    GroupSumPrim (MatchCons meta '[] (ReplicateSum ('Kind.Column qn e ('Kind.Prim t)) dCols) dCols)
  MatchTable _ qCol dCol =
    ErrorWithType "MatchTable" '(qCol, dCol)

-- Construct a @where@ fragment from two types, validating that all fields of the query record and their types are
-- present and matching in the data record
class Where (qCol :: Kind.Column) (query :: *) (dCol :: Kind.Column) (d :: *) where
  queryWhere :: Data.Where d query

instance (
    fields ~ MatchTable (MkQueryMeta qCol dCol) qCol dCol,
    QueryConds fields
  ) => Where qCol query dCol d where
    queryWhere =
      where' (queryConds @fields)
