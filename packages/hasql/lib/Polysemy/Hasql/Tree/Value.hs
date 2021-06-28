module Polysemy.Hasql.Tree.Value where

import Generics.SOP (I (I))
import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdText)
import Polysemy.Db.Data.Rep (Rep)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (QueryRoot (queryRoot), Root (..))
import Polysemy.Db.Tree.Api (TreeConPayload (..), TreePayload (..))
import Polysemy.Db.Tree.Data.Params (Params (Params), TTree)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta (TreeMeta))
import Polysemy.Db.Tree.Effect (D (D), PrimOrTycon, ResolveRep, TreeEffects, TreeEffectsFor)

import Polysemy.Hasql.ColumnType (EffectfulColumnType, effectfulColumnType)
import Polysemy.Hasql.Table.ColumnOptions (ImplicitColumnOptions (..), RepOptions (..), RepToList)
import Polysemy.Hasql.Tree.Table (DbTag, MatchPrim, PrimColumn)

data DbValueTag =
  DbValueTag
  deriving (Eq, Show)

data ColumnData =
  ColumnData {
    name :: Text,
    options :: ColumnOptions
  }
  deriving (Eq, Show)

type DbValueParams = 'Params DbValueTag () I 'False

instance (
    PrimColumn d prim,
    PrimOrTycon DbValueTag (MatchPrim prim d '[] reps) ('D d) effs
  ) => ResolveRep DbValueTag (Rep reps) ('D d) effs

instance TreeEffectsFor DbTag rep d effs => TreeEffects DbValueTag rep d effs

instance (
    EffectfulColumnType name effs d,
    ImplicitColumnOptions d,
    RepOptions (RepToList rep)
  ) => TreePayload DbValueTag ('TreeMeta name rep d) effs ColumnData where
    treePayload =
      ColumnData (effectfulColumnType @name @effs @d) options
      where
        options =
          repOptions @(RepToList rep) <> implicitColumnOptions @d

instance (
    FieldIdText name
  ) => TreeConPayload DbValueTag name ColumnData where
  treeConPayload =
    ColumnData (fieldIdText @name) def

class DbValueTree (rep :: Type) (d :: Type) (tree :: Kind.Tree) | rep d -> tree where
  dbValueTree :: d -> TTree DbValueParams tree

instance (
    Root DbValueParams rep d tree
  ) => DbValueTree rep d tree where
  dbValueTree =
    root @DbValueParams @rep @d . I

class DbValueQueryTree (rep :: Type) (q :: Type) (d :: Type) (tree :: Kind.Tree) | rep q d -> tree where
  dbValueQueryTree :: q -> TTree DbValueParams tree

instance (
    QueryRoot DbValueParams rep q d tree
  ) => DbValueQueryTree rep q d tree where
  dbValueQueryTree =
    queryRoot @DbValueParams @rep @q @d . I
