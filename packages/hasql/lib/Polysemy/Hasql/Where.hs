module Polysemy.Hasql.Where where

import Data.Foldable (foldl1)
import qualified Data.Text as Text
import Generics.SOP (All, K(K), NP, hcollapse, hcpure)
import Hasql.DynamicStatements.Snippet (Snippet)
import Polysemy.Db.Data.FieldId (FieldIdText, quotedFieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (slugString_, symbolString)

import Polysemy.Hasql.Data.SqlCode (SqlCode(SqlCode))
import qualified Polysemy.Hasql.Data.Where as Data (Where(Where))
import Polysemy.Hasql.Where.Dynamic (DynamicQuery, dynamicQuery)
import Polysemy.Hasql.Where.Prepared (QueryWhereColumn(..), concatWhereFields)
import Polysemy.Hasql.Where.Type (
  AsSimple,
  MatchTable,
  MkQueryMeta,
  QCond (SimpleCond, SumPrimCond),
  Segment (SumSegment, ConSegment, FieldSegment),
  WithoutMaybe,
  )

simpleSlug ::
  ∀ (name :: Symbol) .
  KnownSymbol name =>
  String
simpleSlug =
  [text|"#{slugString_ (dropWhile ('_' ==) (symbolString @name))}"|]

where' ::
  [Int -> Text] ->
  (query -> Snippet) ->
  Data.Where a query
where' fields =
  Data.Where (SqlCode (concatWhereFields fields))

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
    base : rest -> [text|(#{base}).#{Text.intercalate "." rest}|]

class QueryCond (field :: QCond) where
  queryCond :: K (Int -> Text) field

instance (
    ReifySegments path,
    QueryWhereColumn q d
  ) => QueryCond ('SimpleCond q d path) where
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
  queryWhere :: Data.Where d query

instance (
    fields ~ MatchTable (MkQueryMeta qTree dTree) qTree dTree,
    All QueryCond fields,
    DynamicQuery qrep query
  ) => Where qrep qTree query dTree d where
    queryWhere =
      where' (queryConds @fields) (dynamicQuery @qrep @query)
