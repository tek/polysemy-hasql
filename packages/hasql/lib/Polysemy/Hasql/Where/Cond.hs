module Polysemy.Hasql.Where.Cond where

import Fcf (Eval, Exp, If, type (@@))
import Fcf.Class.Foldable (Any, FoldMap)
import Fcf.Class.Functor (FMap)
import GHC.TypeLits (ErrorMessage)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Kind.Data.Tree (TreeDataType)
import Polysemy.Db.SOP.Error (Unlines)
import Type.Errors.Pretty (type (%), type (<>))

import Polysemy.Hasql.Where.FlatFields (FieldPath (FieldPath), FlatRoot)
import Polysemy.Hasql.Where.Segment (
  FormatSegments,
  IsSum,
  MatchFieldIds,
  Segment (ConSegment, FieldSegment, SumIndexSegment, SumSegment),
  SegmentId,
  )

data PrimCond =
  PrimCond {
    queryType :: Type,
    dataType :: Type,
    name :: [Segment]
  }

data QCond =
  SimpleCond PrimCond
  |
  SumPrimCond {
    queryType :: Type,
    dataType :: Type,
    names :: [[Segment]]
  }
  |
  TrueCond

type QConds =
  [QCond]

data FormatFieldPath :: FieldPath -> Exp ErrorMessage
type instance Eval (FormatFieldPath ('FieldPath segments _)) =
  FormatSegments segments

type family FormatFieldPaths (paths :: [FieldPath]) :: ErrorMessage where
  FormatFieldPaths paths =
    Unlines (FMap FormatFieldPath @@ paths)

type family GroupNames (q :: Type) (d :: Type) (cs :: [PrimCond]) :: [[Segment]] where
  GroupNames _ _ '[] =
    '[]
  GroupNames q d ('PrimCond q d n : cs) =
    n : GroupNames q d cs
  GroupNames q d ('PrimCond q1 d1 n : _) =
    TypeError (
      "internal [GroupNames]:" %
      "type mismatch between column types of query and data for prim/sum query:" %
      "first constructor: " <> 'ShowType q <> " / " <> 'ShowType d %
      "offending constructor: " <> 'ShowType q1 <> " / " <> 'ShowType d1 %
      "path:" <> 'ShowType n
    )

data GroupConds :: Type -> [FieldPath] -> FieldPath -> [PrimCond] -> Exp QCond
type instance Eval (GroupConds query paths ('FieldPath path _) '[]) =
  TypeError (
    "Unmatched column `" <> FormatSegments path <> "' in query type `" <> 'ShowType query <> "'" %
    "The database type has these columns:" %
    FormatFieldPaths paths
  )
type instance Eval (GroupConds _ _ _ ('PrimCond q d n : cs)) =
  'SumPrimCond q d (n : GroupNames q d cs)

type family MatchSegment (full :: [Segment]) (match :: Bool) (q :: [Segment]) (d :: [Segment]) :: Maybe [Segment] where
  MatchSegment full 'True (_ : qs) (_ : ds) =
    MatchSegments full qs ds
  MatchSegment full 'False ('ConSegment num _ _ : qs) ('ConSegment num _ _ : ds) =
    MatchSegments full qs ds
  MatchSegment _ 'False ('ConSegment _ _ _ : _) ('ConSegment _ _ _ : _) =
    'Nothing
  MatchSegment full 'False qs ('ConSegment _ _ 'True : 'FieldSegment _ : ds) =
    MatchSegments full qs ds
  MatchSegment full 'False qs ('ConSegment _ _ _ : ds) =
    MatchSegments full qs ds
  MatchSegment full 'False qs ('SumSegment _ : ds) =
    MatchSegments full qs ds
  MatchSegment _ _ _ _ =
    'Nothing

type family MatchNames (q :: Segment) (d :: Segment) :: Bool where
  MatchNames ('SumIndexSegment _) ('SumIndexSegment _) =
    'True
  MatchNames _ ('SumIndexSegment _) =
    'False
  MatchNames ('SumIndexSegment _) _ =
    'False
  MatchNames q d =
    MatchFieldIds (Eval (SegmentId q)) (Eval (SegmentId d))

type family MatchSegments (full :: [Segment]) (q :: [Segment]) (d :: [Segment]) :: Maybe [Segment] where
  MatchSegments full '[] '[] =
    'Just full
  MatchSegments full (q : qs) (d : ds) =
    MatchSegment full (MatchNames q d) (q : qs) (d : ds)
  MatchSegments _ _ _ =
    'Nothing

data MatchPaths :: [Segment] -> [Segment] -> Exp (Maybe [Segment])
type instance Eval (MatchPaths q d) =
  MatchSegments d q d

type family Nullability (path :: [Segment]) (q :: Type) :: Type where
  Nullability _ (Maybe q) = Maybe q
  Nullability path q = If (Any IsSum @@ path) (Maybe q) q

data MatchField :: Maybe [Segment] -> q -> d -> Exp [PrimCond]
type instance Eval (MatchField ('Just path) q d) =
  '[ 'PrimCond (Nullability path q) d path]
type instance Eval (MatchField 'Nothing _ _) =
  '[]

data MatchFieldPath :: FieldPath -> FieldPath -> Exp [PrimCond]
type instance Eval (MatchFieldPath ('FieldPath qpath q) ('FieldPath dpath d)) =
  MatchField (Eval (MatchPaths qpath dpath)) q @@ d

data MatchQueryField :: Type -> [FieldPath] -> FieldPath -> Exp QCond
type instance Eval (MatchQueryField query ds q) =
  GroupConds query ds q @@ (FoldMap (MatchFieldPath q) @@ ds)

data MatchQueryFields :: Type -> [FieldPath] -> [FieldPath] -> Exp QConds
type instance Eval (MatchQueryFields query ds qs) =
  FMap (MatchQueryField query ds) @@ qs

type family MatchTable (qTree :: Kind.Tree) (dTree :: Kind.Tree) :: QConds where
  MatchTable qTree dTree =
    MatchQueryFields (TreeDataType qTree) (Eval (FlatRoot dTree)) @@ (Eval (FlatRoot qTree))
