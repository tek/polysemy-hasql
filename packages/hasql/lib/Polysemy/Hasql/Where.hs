module Polysemy.Hasql.Where where

import Data.Foldable (foldl1)
import qualified Data.Text as Text
import Fcf (Eval, Exp, FromMaybe, If, IsJust, Pure, Pure1, type (@@))
import Fcf.Class.Foldable (FoldMap)
import Fcf.Class.Functor (FMap)
import GHC.TypeLits (AppendSymbol)
import Generics.SOP (All, K(K), NP, hcollapse, hcpure)
import Polysemy.Db.Data.Cond (Greater, GreaterOrEq, Less, LessOrEq)
import Polysemy.Db.Data.FieldId (FieldId (NamedField), FieldIdSymbol, FieldIdText, JoinCommaFieldIds, quotedFieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (slugString_, symbolString)
import Polysemy.Db.SOP.Error (ErrorWithType, ErrorWithType2)
import Polysemy.Db.Tree.Data.Effect (ContainsFlatten)
import Type.Errors (ErrorMessage(ShowType), TypeError)
import Type.Errors.Pretty (type (%), type (<>))

import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import qualified Polysemy.Hasql.Data.Where as Data (Where(Where))
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

type family MissingColumn (meta :: QueryMeta) (q :: Kind.Tree) :: k where
  MissingColumn ('QueryMeta rep queryName fieldNames) ('Kind.Tree name _ _) =
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

type family ReplicateSum (qCol :: Kind.Tree) (dCols :: [Kind.Tree]) :: [Kind.Tree] where
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

type family MatchPrim (prefix :: [Segment]) (name :: FieldId) (q :: *) (d :: *) :: Maybe QConds where
  MatchPrim (p : ps) name q d =
    'Just '[ 'SimpleCond (ForceMaybe q) d (('FieldSegment name) : p : ps)]
  MatchPrim prefix name q d =
    'Just '[ 'SimpleCond q d (('FieldSegment name) : prefix)]

type family MatchCon (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Tree) (dCol :: Kind.Tree) :: QConds where
  MatchCon meta prefix ('Kind.Tree _ _ ('Kind.Prod _ q)) ('Kind.Tree conName _ ('Kind.Prod _ d)) =
    FoldMap (MatchQueryColumnE meta ('ConSegment conName : prefix) d) @@ q
  MatchCon meta prefix ('Kind.Tree qname _ ('Kind.Prim q)) ('Kind.Tree conName _ ('Kind.Prod _ d)) =
    MatchQueryColumnE meta ('ConSegment conName : prefix) d @@ ('Kind.Tree qname '[] ('Kind.Prim q))
  -- TODO
  MatchCon _ prefix ('Kind.Tree qname _ ('Kind.Prim q)) ('Kind.Tree qname _ ('Kind.Prim d)) =
    FromMaybe '[] @@ MatchPrim prefix qname q d
  MatchCon _ _ _ _ =
    '[]

type family MatchQCon (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Tree) (dCols :: [Kind.Tree]) :: QConds where
  MatchQCon _ _ _ '[] =
    '[]
  MatchQCon meta prefix qCol (dCol : dCols) =
    MatchCon meta prefix qCol dCol ++ MatchQCon meta prefix qCol dCols

type family MatchCons (meta :: QueryMeta) (prefix :: [Segment]) (qCols :: [Kind.Tree]) (dCols :: [Kind.Tree]) :: QConds where
  MatchCons _ _ '[] _ = '[]
  MatchCons meta prefix (qCol : qCols) (dCol : dCols) =
    MatchCon meta prefix qCol dCol ++ MatchCons meta prefix qCols dCols

type family MatchProd (meta :: QueryMeta) (prefix :: [Segment]) (flatten :: Bool) (qCol :: Kind.Tree) (dCol :: Kind.Tree) :: Maybe QConds where
  MatchProd meta prefix 'True q ('Kind.Tree _ _ ('Kind.Prod  _ cols)) =
    MatchCols meta prefix q cols
  MatchProd meta prefix 'False ('Kind.Tree qname eff ('Kind.Prim q)) ('Kind.Tree name _ ('Kind.Prod _ cols)) =
    MatchCols meta ('FieldSegment name : prefix) ('Kind.Tree qname eff ('Kind.Prim q)) cols
  MatchProd _ _ _ _ _ =
    'Nothing

type family MatchDbType (meta :: QueryMeta) (prefix :: [Segment]) (name :: FieldId) (qCol :: Kind.Tree) (dCol :: Kind.Tree) :: Maybe QConds where
  MatchDbType _ prefix name ('Kind.Tree _ _ ('Kind.Prim q)) ('Kind.Tree _ _ ('Kind.Prim d)) =
    MatchPrim prefix name q d
  MatchDbType meta prefix name ('Kind.Tree _ _ ('Kind.Sum _ qCols)) ('Kind.Tree _ _ ('Kind.Sum _ dCols)) =
    'Just (MatchCons meta ('SumSegment name : prefix) qCols dCols)
  MatchDbType _ _ name ('Kind.Tree _ _ ('Kind.Sum _ _)) ('Kind.Tree _ _ _) =
    'Just (TypeError ("Query column " <> name <> " is a sum type, but the data column is not."))
  MatchDbType _ _ name _ _ =
    'Just (TypeError ("Incompatible column kinds for " <> name))

type family MatchColWithUnderscore (meta :: QueryMeta) (prefix :: [Segment]) (qname :: FieldId) (dname :: FieldId) (dname_ :: Symbol) (qCol :: Kind.Tree) (dCol :: Kind.Tree) :: Maybe QConds where
  MatchColWithUnderscore meta prefix qname qname _ qCol dCol =
    MatchDbType meta prefix qname qCol dCol
  MatchColWithUnderscore meta prefix qname ('NamedField dname) dname qCol dCol =
    MatchDbType meta prefix qname qCol dCol
  MatchColWithUnderscore _ _ _ _ _ _ _ =
    'Nothing

-- |Match a data column against a query column.
-- This decides primarily based upon equality of field names, but it has to compensate for underscore prefixes, which it
-- does by passing an additional argument to 'MatchDbType', the query field name prefixed with underscore.
-- Since product types cannot directly match, the first equation delegates them directly to 'MatchProd', ignoreing the
-- column names.
type family MatchCol' (meta :: QueryMeta) (prefix :: [Segment]) (qname :: FieldId) (dname :: FieldId) (qCol :: Kind.Tree) (dCol :: Kind.Tree) :: Maybe QConds where
  MatchCol' meta prefix _ _ q ('Kind.Tree name effs ('Kind.Prod d cols)) =
    MatchProd meta prefix (ContainsFlatten effs) q ('Kind.Tree name effs ('Kind.Prod d cols))
  MatchCol' meta prefix qname qname qCol dCol =
    MatchDbType meta prefix qname qCol dCol
  MatchCol' meta prefix ('NamedField qname) ('NamedField dname) qCol dCol =
    MatchColWithUnderscore meta prefix ('NamedField qname) ('NamedField dname) (AppendSymbol "_" qname) qCol dCol
  MatchCol' _ _ _ _ _ _ =
    'Nothing

data MatchCol (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Tree) :: Kind.Tree -> Exp (Maybe QConds)
type instance Eval (MatchCol meta prefix ('Kind.Tree qname qEff qCols) ('Kind.Tree dname dEff dCols)) =
  MatchCol' meta prefix qname dname ('Kind.Tree qname qEff qCols) ('Kind.Tree dname dEff dCols)

type family MatchCols (meta :: QueryMeta) (prefix :: [Segment]) (qCol :: Kind.Tree) (dCols :: [Kind.Tree]) :: Maybe QConds where
  MatchCols meta prefix qCol dCols =
    FirstJust (MatchCol meta prefix qCol) @@ dCols

data MatchQueryColumnE (meta :: QueryMeta) (prefix :: [Segment]) :: [Kind.Tree] -> Kind.Tree -> Exp QConds
type instance Eval (MatchQueryColumnE meta prefix dCols qCol) =
  FromMaybe (MissingColumn meta qCol) @@ MatchCols meta prefix qCol dCols

data QueryMeta =
  QueryMeta {
    rep :: Kind.Tree,
    query :: FieldId,
    fields :: [FieldId]
  }

data DbTypeFieldNames :: Kind.Tree -> Exp [FieldId]

type instance Eval (DbTypeFieldNames ('Kind.Tree name _ ('Kind.Prim _))) =
  '[name]
type instance Eval (DbTypeFieldNames ('Kind.Tree _ _ ('Kind.Prod _ cols))) =
  FoldMap DbTypeFieldNames @@ cols
type instance Eval (DbTypeFieldNames ('Kind.Tree name _ ('Kind.Sum _ cols))) =
  name : FoldMap DbTypeFieldNames @@ cols

type family MkQueryMeta (qCol :: Kind.Tree) (dCol :: Kind.Tree) :: QueryMeta where
  MkQueryMeta ('Kind.Tree name _ _) dCol =
    'QueryMeta dCol name (DbTypeFieldNames @@ dCol)

type family MatchTable (meta :: QueryMeta) (qCol :: Kind.Tree) (dCol :: Kind.Tree) :: QConds where
  MatchTable meta ('Kind.Tree _ _ ('Kind.Prod _ qCols)) ('Kind.Tree _ _ ('Kind.Prod _ dCols)) =
    FoldMap (MatchQueryColumnE meta '[] dCols) @@ qCols
  MatchTable meta ('Kind.Tree n e ('Kind.Prim t)) ('Kind.Tree _ _ ('Kind.Prod _ dCols)) =
    MatchQueryColumnE meta '[] dCols @@ ('Kind.Tree n e ('Kind.Prim t))
  MatchTable meta ('Kind.Tree _ _ ('Kind.Sum _ qCols)) ('Kind.Tree _ _ ('Kind.Sum _ dCols)) =
    MatchCons meta '[] qCols dCols
  MatchTable meta ('Kind.Tree qn e ('Kind.Prim t)) ('Kind.Tree _ _ ('Kind.Sum _ dCols)) =
    GroupSumPrim (MatchCons meta '[] (ReplicateSum ('Kind.Tree qn e ('Kind.Prim t)) dCols) dCols)
  MatchTable _ qCol dCol =
    ErrorWithType "MatchTable" '(qCol, dCol)

-- Construct a @where@ fragment from two types, validating that all fields of the query record and their types are
-- present and matching in the data record
class Where (qCol :: Kind.Tree) (query :: *) (dCol :: Kind.Tree) (d :: *) where
  queryWhere :: Data.Where d query

instance (
    fields ~ MatchTable (MkQueryMeta qCol dCol) qCol dCol,
    QueryConds fields
  ) => Where qCol query dCol d where
    queryWhere =
      where' (queryConds @fields)
