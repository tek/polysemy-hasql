module Polysemy.Hasql.Where.Segment where

import Fcf (Eval, Exp, type (<=<), type (@@), type (||))
import Fcf.Alg.Symbol (type (==))
import Fcf.Class.Functor (FMap)
import GHC.TypeLits (AppendSymbol)
import Polysemy.Db.Data.FieldId (FieldId (NamedField, NumberedField), FieldIdSymbol)
import Polysemy.Db.SOP.Error (JoinError)
import Type.Errors (ErrorMessage (Text), TypeError)

data Segment =
  FieldSegment FieldId
  |
  SumSegment FieldId
  |
  ConSegment {
    num :: Nat,
    conId :: FieldId,
    unary :: Bool
  }
  |
  SumIndexSegment Type

data SegmentId :: Segment -> Exp FieldId
type instance Eval (SegmentId ('FieldSegment id)) = id
type instance Eval (SegmentId ('SumSegment id)) = id
type instance Eval (SegmentId ('ConSegment _ id _)) = id
type instance Eval (SegmentId ('SumIndexSegment _)) = TypeError ('Text "internal: SegmentId SumIndexSegment")

data IsSum :: Segment -> Exp Bool
type instance Eval (IsSum ('SumSegment _)) = 'True
type instance Eval (IsSum ('ConSegment _ _ _)) = 'True
type instance Eval (IsSum ('FieldSegment _)) = 'False
type instance Eval (IsSum ('SumIndexSegment _)) = 'False

type family MatchFieldIds (q :: FieldId) (d :: FieldId) :: Bool where
  MatchFieldIds ('NamedField q) ('NamedField d) =
    Eval (Eval (Eval (q == d) || Eval (AppendSymbol "_" q == d)) || Eval (AppendSymbol q "_" == d))
  MatchFieldIds ('NumberedField _ d) ('NumberedField _ d) =
    'True
  MatchFieldIds _  _ =
    'False

type family FormatSegments (segs :: [Segment]) :: ErrorMessage where
  FormatSegments segs =
    JoinError ('Text ".") (FMap (FieldIdSymbol <=< SegmentId) @@ segs)
