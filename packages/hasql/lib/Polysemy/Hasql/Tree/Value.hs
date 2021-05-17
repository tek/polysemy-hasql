module Polysemy.Hasql.Tree.Value where

import Generics.SOP (I(I))
import Polysemy.Db.Data.Column (ForcePrim, Rep)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdText)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (Root(..))
import Polysemy.Db.Tree.Api (TreeConPayload(..), TreePayload(..))
import Polysemy.Db.Tree.Data.Params (Params(Params), TTree)
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Effect (D(D), PrimOrTycon, ResolveRep, TreeEffects, TreeEffectsFor, WithPrim)

import Polysemy.Hasql.Column.Effect (PrimColumn)
import Polysemy.Hasql.Column.Tree (DbTag)
import Polysemy.Hasql.ColumnType (EffectfulColumnType, effectfulColumnType)
import Polysemy.Hasql.Table.ColumnOptions (ImplicitColumnOptions(..), RepOptions(..), RepToList)

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

type family MatchPrim (global :: Bool) (d :: Type) (pre :: [*]) (reps :: [*]) :: Either [*] [*] where
  MatchPrim 'True _ pre '[] = 'Right (WithPrim pre)
  MatchPrim 'False _ pre '[] = 'Left pre
  MatchPrim _ d pre (ForcePrim d : rest) = 'Right (WithPrim (pre ++ rest))
  MatchPrim global d pre (rep : rest) = MatchPrim global d (pre ++ '[rep]) rest

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
    Root rep DbValueParams d tree
  ) => DbValueTree rep d tree where
  dbValueTree =
    root @rep @DbValueParams @d . I
