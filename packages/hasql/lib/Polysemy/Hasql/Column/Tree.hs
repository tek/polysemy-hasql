module Polysemy.Hasql.Column.Tree where

import Generics.SOP (K(K))
import Polysemy.Db.Data.Column (ForcePrim, Rep)
import Polysemy.Db.Data.FieldId (FieldIdText, fieldIdText)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.Tree (ProdForSumTree, Root(..))
import Polysemy.Db.Tree.Api (TreeConPayload(..), TreePayload(..), TreePrim(..))
import Polysemy.Db.Tree.Data.Params (Params(Params))
import Polysemy.Db.Tree.Data.TreeMeta (TreeMeta(TreeMeta))
import Polysemy.Db.Tree.Effect (D(D), PrimOrTycon, ResolveRep, TreeEffects, TreeEffectsFor, WithPrim)
import qualified Polysemy.Db.Type.Data.Tree as Type
import Polysemy.Db.Type.Data.Tree (ColumnData(ColumnData))

import Polysemy.Hasql.Column.Effect (PrimColumn)
import Polysemy.Hasql.ColumnType (EffectfulColumnType, effectfulColumnType)
import Polysemy.Hasql.Table.ColumnOptions (
  ImplicitColumnOptions,
  RepOptions,
  RepToList,
  implicitColumnOptions,
  repOptions,
  )

data DbTag =
  DbTag
  deriving (Eq, Show)

type DbTree = Type.Tree ColumnData Proxy
type DbParams = 'Params DbTag ColumnData Proxy (K ())
instance ProdForSumTree DbTag 'True

instance TreePrim DbTag Proxy a name d where
  treePrim _ =
    Proxy

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
  ) => TreePayload DbTag ColumnData a ('TreeMeta name rep d) effs where
    treePayload _ =
      ColumnData (effectfulColumnType @name @effs @d) options
      where
        options =
          repOptions @(RepToList rep) <> implicitColumnOptions @d

instance (
    FieldIdText name
  ) => TreeConPayload DbTag name ColumnData where
  treeConPayload =
    ColumnData (fieldIdText @name) def

class TableColumn (rep :: Type) (d :: Type) (tree :: Kind.Tree) | rep d -> tree where
  tableColumn :: DbTree tree

instance (
    Root rep DbParams d tree
  ) => TableColumn rep d tree where
  tableColumn =
    root @rep @DbParams @d (K ())
