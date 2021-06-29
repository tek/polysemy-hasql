module Polysemy.Hasql.Where where

import Data.Foldable (foldl1)
import qualified Data.Text as Text
import Fcf (Eval, Pure1, type (<=<), type (@@))
import Fcf.Class.Functor (FMap)
import Generics.SOP (All, K (K), NP, hcollapse, hcpure)
import Hasql.DynamicStatements.Snippet (Snippet)
import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField), FieldIdText, quotedFieldId)
import Polysemy.Db.Data.Uid (Uid)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName, slugString_, symbolString)
import Polysemy.Db.Text.Quote (dquote)

import Polysemy.Hasql.Data.SqlCode (SqlCode (SqlCode))
import qualified Polysemy.Hasql.Data.Where as Data (Where (Where))
import Polysemy.Hasql.Table.QueryParam (QueryValueNoN)
import Polysemy.Hasql.Table.SumIndex (sumIndexIdentifier)
import Polysemy.Hasql.Where.Cond (
  MatchTable,
  PrimCond (PrimCond),
  QCond (SimpleCond, SumPrimCond),
  )
import Polysemy.Hasql.Where.Dynamic (DynamicQuery, dynamicQuery, field)
import Polysemy.Hasql.Where.Prepared (QueryWhereColumn (..), concatWhereFields)
import Polysemy.Hasql.Where.Segment (Segment (ConSegment, FieldSegment, SumIndexSegment), SegmentId)

type family AsSimple (q :: Type) (d :: Type) (ns :: [[Segment]]) :: [QCond] where
  AsSimple q d ns =
    FMap (Pure1 'SimpleCond <=< Pure1 ('PrimCond q d)) @@ ns

type family WithoutMaybe (d :: Type) :: Type where
  WithoutMaybe (Maybe d) = d
  WithoutMaybe d = d

simpleSlug ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  String
simpleSlug =
  [text|"#{slugString_ (dropWhile ('_' ==) (symbolString @name))}"|]

where' ::
  [Int -> Text] ->
  (query -> Snippet) ->
  Data.Where query d
where' fields =
  Data.Where (SqlCode (concatWhereFields fields))

class ReifySegments (s :: [Segment]) where
  reifySegments :: [Text]

instance ReifySegments '[] where
  reifySegments =
    mempty

instance {-# overlappable #-} (
    id ~ Eval (SegmentId s),
    FieldIdText id,
    ReifySegments ss
  ) => ReifySegments (s : ss) where
  reifySegments =
    quotedFieldId @id : reifySegments @ss

instance (
    DataName d name
  ) => ReifySegments '[ 'SumIndexSegment d] where
    reifySegments =
      [dquote (sumIndexIdentifier @d)]

instance (
    FieldIdText ('NamedField id),
    ReifySegments ss
  ) => ReifySegments ('ConSegment _num _id 'True : 'FieldSegment ('NamedField id) : ss) where
    reifySegments =
      quotedFieldId @('NamedField id) : reifySegments @ss

instance (
    FieldIdText id,
    ReifySegments ss
  ) => ReifySegments ('ConSegment _cnum id 'True : 'FieldSegment ('NumberedField _n _num) : ss) where
    reifySegments =
      quotedFieldId @id : reifySegments @ss

reifyPath ::
  ∀ s .
  ReifySegments s =>
  Text
reifyPath =
  case reifySegments @s of
    [] -> ""
    [prim] -> prim
    base : rest -> [text|(#{base}).#{Text.intercalate "." rest}|]

class QueryCond (field :: QCond) where
  queryCond :: K (Int -> Text) field

instance (
    ReifySegments path,
    QueryWhereColumn q d
  ) => QueryCond ('SimpleCond ('PrimCond q d path)) where
  queryCond =
    K (queryWhereColumn @q @d (reifyPath @path))

queryConds ::
  ∀ fields .
  All QueryCond fields =>
  [Int -> Text]
queryConds =
  hcollapse (hcpure (Proxy @QueryCond) queryCond :: NP (K (Int -> Text)) fields)

instance (
    simple ~ AsSimple q (WithoutMaybe d) ns,
    All QueryCond simple
  ) => QueryCond ('SumPrimCond q d ns) where
  queryCond =
    K (fromMaybe (const "") (foldl1 (\ z a -> \ i -> z i <> " or " <> a i) <$> nonEmpty (queryConds @simple)))

-- Construct a @where@ fragment from two types, validating that all fields of the query record and their types are
-- present and matching in the data record
class Where (qrep :: Type) (qTree :: Kind.Tree) (query :: Type) (dTree :: Kind.Tree) (d :: Type) where
  queryWhere :: Data.Where query d

instance (
    fields ~ MatchTable qTree dTree,
    All QueryCond fields,
    DynamicQuery qrep query d
  ) => Where qrep qTree query dTree d where
    queryWhere =
      where' (queryConds @fields) (dynamicQuery @qrep @query @d)

uidWhere ::
  ∀ effs i d .
  QueryValueNoN effs i =>
  Data.Where i (Uid i d)
uidWhere =
  Data.Where (SqlCode "id = $1") (field @"id" @effs)
