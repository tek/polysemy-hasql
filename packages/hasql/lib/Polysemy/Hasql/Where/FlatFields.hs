{-# language StandaloneKindSignatures #-}

module Polysemy.Hasql.Where.FlatFields where

import Fcf (Eval, Exp, type (@@))
import Fcf.Class.Foldable (ConcatMap)
import Fcf.Data.List (Reverse)
import Polysemy.Db.Data.FieldId (FieldId)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Error (ErrorWithType)
import Polysemy.Db.Tree.Data.Effect (ContainsFlatten, Newtype)

import Polysemy.Hasql.Where.Segment (Segment (ConSegment, FieldSegment, SumIndexSegment, SumSegment))

data FieldPath =
  FieldPath {
    segments :: [Segment],
    tpe :: Type
  }

type family AddPrefix (root :: Bool) (cons :: FieldId -> Segment) (name :: Maybe FieldId) (prefix :: [Segment]) :: [Segment] where
  AddPrefix 'False cons ('Just name) prefix =
    cons name : prefix
  AddPrefix _ _ _ prefix =
    prefix

type family PrefixUnlessFlatten (contains :: Bool) (field :: Maybe FieldId) :: Maybe FieldId where
  PrefixUnlessFlatten 'False field =
    field
  PrefixUnlessFlatten _ _ =
    'Nothing

type family ProductPrefix (field :: Maybe FieldId) (effs :: [Type]) :: Maybe FieldId where
  ProductPrefix field effs =
    PrefixUnlessFlatten (ContainsFlatten effs) field

data FlatCon :: [Segment] -> Kind.Con -> Exp [FieldPath]
type instance Eval (FlatCon prefix ('Kind.Con num name trees)) =
  ConcatMap (FlatTree ('ConSegment num name 'False : prefix)) @@ trees
type instance Eval (FlatCon prefix ('Kind.ConUna num conId ('Kind.Tree fieldId effs node))) =
  FlatNode 'False 'Nothing ('FieldSegment fieldId : 'ConSegment num conId 'True : prefix) effs @@ node

type family UnwrapNewtype (a :: Type) (effs :: [Type]) :: Type where
  UnwrapNewtype a '[] =
    a
  UnwrapNewtype a (Newtype a d : effs) =
    UnwrapNewtype d effs
  UnwrapNewtype a (_ : effs) =
    UnwrapNewtype a effs

type family PrimNode (root :: Bool) (name :: Maybe FieldId) (prefix :: [Segment]) (effs :: [Type]) (d :: Type) :: FieldPath where
  PrimNode 'True ('Just name) '[] effs d =
    'FieldPath '[ 'FieldSegment name] (UnwrapNewtype d effs)
  PrimNode root name prefix effs d =
    'FieldPath (Reverse @@ (AddPrefix root 'FieldSegment name prefix)) (UnwrapNewtype d effs)

type SumIndex :: Type -> [Segment] -> FieldPath
type SumIndex sum prefix =
  'FieldPath (Reverse @@ ('SumIndexSegment sum : prefix)) Int

data SumNode :: Type -> [Segment] -> [Kind.Con] -> Exp [FieldPath]
type instance Eval (SumNode sum prefix cons) =
  SumIndex sum prefix : ConcatMap (FlatCon prefix) @@ cons

data FlatNode :: Bool -> Maybe FieldId -> [Segment] -> [Type] -> Kind.Node -> Exp [FieldPath]
type instance Eval (FlatNode root name prefix effs ('Kind.Prim d)) =
  '[PrimNode root name prefix effs d]
type instance Eval (FlatNode root name prefix effs ('Kind.Prod _ trees)) =
  ConcatMap (FlatTree (AddPrefix root 'FieldSegment (ProductPrefix name effs) prefix)) @@ trees
type instance Eval (FlatNode root name prefix effs ('Kind.SumProd d cons)) =
  Eval (SumNode d (AddPrefix root 'SumSegment (ProductPrefix name effs) prefix) cons)
type instance Eval (FlatNode _ _ _ _ ('Kind.Sum d _)) =
  ErrorWithType "Cannot use Kind.Sum for 'where' clause" d

data FlatTree :: [Segment] -> Kind.Tree -> Exp [FieldPath]
type instance Eval (FlatTree prefix ('Kind.Tree name effs node)) =
  FlatNode 'False ('Just name) prefix effs @@ node

data FlatRoot :: Kind.Tree -> Exp [FieldPath]
type instance Eval (FlatRoot ('Kind.Tree name effs node)) =
  FlatNode 'True ('Just name) '[] effs @@ node
