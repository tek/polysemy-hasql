module Polysemy.Hasql.Table.DataColumn where

import Generics.SOP (All, AllN, CollapseTo, HAp, HCollapse (hcollapse), K (K), NP, Prod, hcmap)
import Generics.SOP.Constraint (SListIN)
import Polysemy.Db.Data.FieldId (FieldId (NamedField), FieldIdText, fieldIdText)
import qualified Polysemy.Db.Kind.Data.Tree as Kind
import Polysemy.Db.SOP.Constraint (DataName)
import Polysemy.Db.Text.DbIdentifier (dbIdentifierT, quotedDbId)
import Polysemy.Db.Tree.Data.Effect (ContainsFlatten)
import qualified Polysemy.Db.Type.Data.Tree as Type

import qualified Polysemy.Hasql.Data.DbType as Data
import Polysemy.Hasql.Data.DbType (Name (Name), Selector (Selector), TypeName (CompositeTypeName, PrimTypeName))
import Polysemy.Hasql.Table.SumIndex (sumIndexIdentifier)
import Polysemy.Hasql.Tree.Table (ColumnData (ColumnData), TableCon, TableNode, TableRoot, TableTree, tableRoot)

data ColumnPrefix =
  InitPrefix
  |
  TablePrefix
  |
  BasePrefix Text
  |
  TypePrefix Text
  deriving (Eq, Show)

addPrefix ::
  Text ->
  ColumnPrefix ->
  ColumnPrefix
addPrefix segment = \case
  InitPrefix -> TablePrefix
  TablePrefix -> BasePrefix (quotedDbId segment)
  BasePrefix name -> TypePrefix [text|(#{name}).#{quotedDbId segment}|]
  TypePrefix prefix -> TypePrefix [text|#{prefix}.#{quotedDbId segment}|]

prefixed :: Text -> ColumnPrefix -> Text
prefixed name = \case
  InitPrefix -> quotedDbId name
  TablePrefix -> quotedDbId name
  BasePrefix prefix -> [text|(#{prefix}).#{quotedDbId name}|]
  TypePrefix prefix -> [text|#{prefix}.#{quotedDbId name}|]

mapColumns ::
  ∀ k (f :: (k -> Type) -> [k] -> Type) (a :: Type) (trees :: [k]) (cls :: k -> Constraint) n .
  HCollapse f =>
  SListIN f trees =>
  AllN (Prod f) cls trees =>
  HAp f =>
  (∀ x . cls x => n x -> a) ->
  f n trees ->
  CollapseTo f a
mapColumns f trees =
  hcollapse @_ @_ @f @_ @a (hcmap (Proxy @cls) (K . f) trees)

sumProd ::
  ∀ d trees name .
  DataName d name =>
  All DataDbCon trees =>
  ColumnPrefix ->
  TableNode ('Kind.SumProd d trees) ->
  [Data.Column]
sumProd prefix (Type.SumProd _ trees) =
  indexColumn : mapColumns @_ @NP @_ @trees @DataDbCon (dataDbCon prefix) trees
  where
    indexColumn =
      Data.Column (Name indexName) (Selector (prefixed indexName prefix)) (PrimTypeName "bigint") def Data.Prim
    indexName =
      sumIndexIdentifier @d

class ColumnTypeName (node :: Kind.Node) where
  columnTypeName :: Text -> TypeName

instance ColumnTypeName ('Kind.Prim d) where
  columnTypeName =
    PrimTypeName

instance ColumnTypeName ('Kind.Prod tpe sub) where
  columnTypeName =
    CompositeTypeName

instance ColumnTypeName ('Kind.SumProd tpe sub) where
  columnTypeName =
    CompositeTypeName

class DataProduct (flatten :: Bool) (tree :: Kind.Tree) where
  dataProduct :: ColumnPrefix -> TableTree tree -> [Data.Column]

instance (
    All DataProductOrFlatten trees
  ) => DataProduct 'True ('Kind.Tree name effs ('Kind.Prod d trees)) where
    dataProduct prefix (Type.Tree _ (Type.Prod _ trees)) =
      join (mapColumns @_ @NP @_ @trees @DataProductOrFlatten (dataProductOrFlatten prefix) trees)

instance (
    DataName d dname,
    All DataDbCon trees
  ) => DataProduct 'True ('Kind.Tree name effs ('Kind.SumProd d trees)) where
    dataProduct prefix (Type.Tree _ node) =
      sumProd prefix node

instance DataProduct 'False ('Kind.Tree name effs ('Kind.Prim ())) where
    dataProduct =
      mempty

instance {-# overlappable #-} (
    DataColumn ('Kind.Tree name effs tpe)
  ) => DataProduct 'False ('Kind.Tree name effs tpe) where
    dataProduct =
      pure .: dataColumn

class DataProductOrFlatten (tree :: Kind.Tree) where
  dataProductOrFlatten :: ColumnPrefix -> TableTree tree -> [Data.Column]

instance (
    tree ~ 'Kind.Tree name effs tpe,
    flatten ~ ContainsFlatten effs,
    DataProduct flatten tree
  ) => DataProductOrFlatten tree where
    dataProductOrFlatten =
      dataProduct @flatten

class DataDbCon (con :: Kind.Con) where
  dataDbCon :: ColumnPrefix -> TableCon con -> Data.Column

instance (
    All (DataProduct 'False) trees,
    FieldIdText name
  ) => DataDbCon ('Kind.Con num name trees) where
    dataDbCon prefix (Type.Con trees) =
      Data.Column (Name (dbIdentifierT name)) (Selector (prefixed name prefix)) (CompositeTypeName name) def dbType
      where
        name =
          fieldIdText @name
        dbType =
          Data.Prod (join (mapColumns @_ @NP @_ @trees @(DataProduct 'False) (dataProduct @'False newPrefix) trees))
        newPrefix =
          addPrefix name prefix

type family ConUnaName (cname :: FieldId) (fname :: FieldId) :: FieldId where
  ConUnaName _ ('NamedField name) =
    'NamedField name
  ConUnaName cname _ =
    cname

instance (
    DataDbNode node,
    name ~ ConUnaName cname fname,
    FieldIdText name,
    ColumnTypeName node
  ) => DataDbCon ('Kind.ConUna num cname ('Kind.Tree fname effs node)) where
    dataDbCon prefix (Type.ConUna (Type.Tree (ColumnData tpe options) tree)) =
      Data.Column (Name (dbIdentifierT name)) selector typeName options (dataDbNode newPrefix tree)
      where
        newPrefix =
          addPrefix name prefix
        typeName =
          columnTypeName @node tpe
        name =
          fieldIdText @name
        selector =
          Selector (prefixed name prefix)

class DataDbNode (t :: Kind.Node) where
  dataDbNode :: ColumnPrefix -> TableNode t -> Data.DbType

instance (
    All DataProductOrFlatten trees
  ) => DataDbNode ('Kind.Prod d trees) where
  dataDbNode prefix (Type.Prod _ trees) =
    Data.Prod (join (mapColumns @_ @NP @_ @trees @DataProductOrFlatten (dataProductOrFlatten prefix) trees))

instance (
    DataName d name,
    All DataDbCon trees
  ) => DataDbNode ('Kind.SumProd d trees) where
  dataDbNode prefix node =
    Data.Prod (sumProd prefix node)

instance DataDbNode ('Kind.Prim d) where
    dataDbNode _ (Type.Prim _) =
      Data.Prim

class DataColumn (tree :: Kind.Tree) where
  dataColumn :: ColumnPrefix -> TableTree tree -> Data.Column

instance (
    ColumnTypeName node,
    DataDbNode node,
    FieldIdText name
  ) => DataColumn ('Kind.Tree name effs node) where
    dataColumn prefix (Type.Tree (ColumnData tpe options) dbType) =
      Data.Column (Name (dbIdentifierT name)) (Selector (prefixed name prefix)) typeName options dataType
      where
        dataType =
          dataDbNode (addPrefix name prefix) dbType
        typeName =
          columnTypeName @node tpe
        name =
          fieldIdText @name

class DataTable (tree :: Kind.Tree) where
  dataTable :: TableTree tree -> Data.Column

instance (
    DataColumn tree
  ) => DataTable tree where
  dataTable =
    dataColumn @tree InitPrefix

class TableStructure (rep :: Type) (d :: Type) where
  tableStructure :: Data.Column

instance (
    TableRoot rep d tree,
    DataTable tree
  ) => TableStructure rep d where
    tableStructure =
      dataTable (tableRoot @rep @d)
