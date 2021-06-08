{-# options_ghc -Wno-redundant-constraints #-}

module Polysemy.Hasql.Test.Tree.DeriveNewtypePartialTest where

import Path (File, Path, Rel)
import Polysemy.Db.Data.FieldId (FieldId (NamedField))
import Polysemy.Db.Data.PartialField (Partially)
import Polysemy.Db.Data.Rep (Auto, Prim, Rep)
import Polysemy.Db.Kind.Data.Tree (PrimTree, PrimTreeWith, ProdRoot)
import Polysemy.Db.Tree.Data.Effect (Newtype)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta (TreeMeta))
import Polysemy.Db.Tree.Effect (D (D), DefaultEffects, Effs (Effs), ResolveRep, TreeEffects, TreeEffectsFor)
import Polysemy.Db.Tree.Partial (PartialParams, PartialTag)
import Polysemy.Test (UnitTest)

newtype Tex =
  Tex { unTex :: Text }
  deriving (Eq, Show, Generic)
  deriving newtype (IsString)

data Dat =
  Dat {
    txt :: Tex,
    path :: Path Rel File
  }
  deriving (Eq, Show, Generic)

newtypePartialDerivation ::
  tag ~ DefaultEffects =>
  d ~ Dat =>
  p ~ PartialParams =>
  meta ~ 'TreeMeta ('NamedField "Dat") Auto Dat =>
  ResolveRep tag (Rep '[]) ('D Tex) ('Effs effs) =>
  TreeEffectsFor tag Auto Tex effs =>
  TreeEffects PartialTag Auto Tex effs =>
  effs ~ '[Newtype Tex Text, Prim] =>
  Partially d tree =>
  tree ~ ProdRoot Dat [
    PrimTreeWith "txt" '[Newtype Tex Text, Prim] Tex,
    PrimTree "path" (Path Rel File)
  ] =>
  ()
newtypePartialDerivation =
  ()

test_deriveNewtypePartial :: UnitTest
test_deriveNewtypePartial =
  pure newtypePartialDerivation
