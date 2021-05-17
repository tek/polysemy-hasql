module Polysemy.Hasql.Tree.Table where

import Polysemy.Db.Data.Column (ForcePrim, Rep)
import Polysemy.Db.Data.ColumnOptions (ColumnOptions)
import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdText)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (Root(..))
import Polysemy.Db.Tree.Api (TreeConPayload(..), TreePayload(..))
import Polysemy.Db.Tree.Data.Params (Params(Params))
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Effect (D(D), PrimOrTycon, ResolveRep, TreeEffects, TreeEffectsFor, WithPrim)
import qualified Polysemy.Db.Type.Data.Tree as Type

import Polysemy.Hasql.Column.Effect (PrimColumn)
import Polysemy.Hasql.ColumnType (EffectfulColumnType, effectfulColumnType)
import Polysemy.Hasql.Table.ColumnOptions (ImplicitColumnOptions(..), RepOptions(..), RepToList)

data DbTag =
  DbTag
  deriving (Eq, Show)

data ColumnData =
  ColumnData {
    name :: Text,
    options :: ColumnOptions
  }
  deriving (Eq, Show)

type TableNode = Type.Node ColumnData Proxy
type TableTree = Type.Tree ColumnData Proxy
type TableCon = Type.Con ColumnData Proxy
type TableParams = 'Params DbTag ColumnData Proxy 'True

type family MatchPrim (global :: Bool) (d :: Type) (pre :: [*]) (reps :: [*]) :: Either [*] [*] where
  MatchPrim 'True _ pre '[] = 'Right (WithPrim pre)
  MatchPrim 'False _ pre '[] = 'Left pre
  MatchPrim _ d pre (ForcePrim d : rest) = 'Right (WithPrim (pre ++ rest))
  MatchPrim global d pre (rep : rest) = MatchPrim global d (pre ++ '[rep]) rest

instance (
    PrimColumn d prim,
    PrimOrTycon DbTag (MatchPrim prim d '[] reps) ('D d) effs
  ) => ResolveRep DbTag (Rep reps) ('D d) effs

instance TreeEffectsFor DbTag rep d effs => TreeEffects DbTag rep d effs

instance (
    EffectfulColumnType name effs d,
    ImplicitColumnOptions d,
    RepOptions (RepToList rep)
  ) => TreePayload DbTag ('TreeMeta name rep d) effs ColumnData where
    treePayload =
      ColumnData (effectfulColumnType @name @effs @d) options
      where
        options =
          repOptions @(RepToList rep) <> implicitColumnOptions @d

instance (
    FieldIdText name
  ) => TreeConPayload DbTag name ColumnData where
  treeConPayload =
    ColumnData (fieldIdText @name) def

class TableRoot (rep :: Type) (d :: Type) (tree :: Kind.Tree) | rep d -> tree where
  tableRoot :: TableTree tree

instance (
    Root rep TableParams d tree
  ) => TableRoot rep d tree where
  tableRoot =
    root @rep @TableParams @d Proxy
