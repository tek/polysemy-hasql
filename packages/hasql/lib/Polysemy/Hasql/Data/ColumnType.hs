module Polysemy.Hasql.Data.ColumnType where

import Type.Errors (ErrorMessage(Text, ShowType), TypeError)
import Type.Errors.Pretty (type (<>))

import Polysemy.Db.Data.Column (Auto)

data AutoInternal =
  AutoInternal
  deriving (Eq, Show)

type Auto' = '[AutoInternal]

type family HeadRep rep where
  HeadRep Auto' = Auto
  HeadRep (r : r1 : reps) = r

type family TailRep rep where
  TailRep Auto' = Auto'
  TailRep (r : r1 : reps) = r1 : reps
  TailRep '[r] = TypeError ('Text "too few types in rep for GenColumns after '" <> 'ShowType r <> "'")
