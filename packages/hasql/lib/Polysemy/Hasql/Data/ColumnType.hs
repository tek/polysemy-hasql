module Polysemy.Hasql.Data.ColumnType where

import Type.Errors (ErrorMessage(Text), TypeError)

import Polysemy.Db.Data.Column (Auto)

data AutoInternal =
  AutoInternal
  deriving (Eq, Show)

type Auto' = '[AutoInternal]
type Auto'' = '[ '[AutoInternal]]

type family HeadRep rep where
  HeadRep Auto' = Auto
  HeadRep (r : reps) = r

type family TailRep rep where
  TailRep Auto' = Auto'
  TailRep (r : reps) = reps
  TailRep '[] = TypeError ('Text "too few types in rep for Columns")

type family HeadRep2 rep where
  HeadRep2 Auto'' = Auto'
  HeadRep2 (r : reps) = r

type family TailRep2 rep where
  TailRep2 Auto'' = Auto''
  TailRep2 (r : reps) = reps
  TailRep2 '[] = TypeError ('Text "too few types in rep for Columns")
