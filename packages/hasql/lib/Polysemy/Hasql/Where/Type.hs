module Polysemy.Hasql.Where.Type where

-- import Fcf (Eval, Exp, FromMaybe, Pure1, type (@@))
-- import Fcf.Class.Foldable (Concat, FoldMap)
-- import Fcf.Class.Functor (FMap)
-- import GHC.TypeLits (AppendSymbol)
-- import Polysemy.Db.Data.FieldId (FieldId (NamedField), FieldIdSymbol, JoinCommaFieldIds)
-- import qualified Polysemy.Db.Kind.Data.Tree as Kind
-- import Polysemy.Db.Kind.Data.Tree (NodeDataType)
-- import Polysemy.Db.SOP.Error (ErrorWithType)
-- import Polysemy.Db.SOP.List (FirstJust)
-- import Polysemy.Db.Tree.Data.Effect (ContainsFlatten, Newtype)
-- import Type.Errors (ErrorMessage(ShowType), TypeError)
-- import Type.Errors.Pretty (type (%), type (<>))

-- import Polysemy.Hasql.Where.QueryMeta (QueryMeta(QueryMeta))

-- data FieldName =
--   FieldName Symbol

-- data ColumnName =
--   SumName Symbol Symbol Symbol
--   |
--   SimpleName Symbol

-- data Field =
--   Field Type FieldName

-- type Fields =
--   [Field]

-- data Segment =
--   FieldSegment FieldId
--   |
--   SumSegment FieldId
--   |
--   ConSegment FieldId

-- data QCond =
--   SimpleCond {
--     queryType :: Type,
--     dataType :: Type,
--     name :: [Segment]
--   }
--   |
--   SumPrimCond {
--     queryType :: Type,
--     dataType :: Type,
--     names :: [[Segment]]
--   }
--   |
--   TrueCond

-- type family AsSimple (q :: Type) (d :: Type) (ns :: [[Segment]]) :: [QCond] where
--   AsSimple q d ns =
--     FMap (Pure1 ('SimpleCond q d)) @@ ns

-- type family WithoutMaybe (d :: Type) :: Type where
--   WithoutMaybe (Maybe d) = d
--   WithoutMaybe d = d

-- data TableField =
--   TableField {
--     rep :: Type,
--     field :: Field
--   }

-- type QConds =
--   [QCond]

-- data Table =
--   Table {
--     rowType :: Type,
--     fields :: [TableField],
--     fieldNames :: [Symbol]
--   }

-- data QueryTable =
--   QueryTable {
--     queryName :: Symbol,
--     table :: Table
--   }

-- type family MissingColumn (meta :: QueryMeta) (q :: Kind.Tree) :: k where
--   MissingColumn ('QueryMeta rep queryName fieldNames) ('Kind.Tree name _ _) =
--     TypeError ((
--       "Unmatched column `" <> FieldIdSymbol @@ name <> "' in query type `" <> FieldIdSymbol @@ queryName <> "'" %
--       "The database type has these columns:" %
--       JoinCommaFieldIds fieldNames
--     ) % 'ShowType rep)

-- type family ForceMaybe (d :: Type) :: Type where
--   ForceMaybe (Maybe d) = Maybe d
--   ForceMaybe d = Maybe d

-- type family ForceMaybePrim (t :: Kind.Tree) :: Kind.Tree where
--   ForceMaybePrim ('Kind.Tree n eff ('Kind.Prim (Maybe d))) = 'Kind.Tree n eff ('Kind.Prim (Maybe d))
--   ForceMaybePrim ('Kind.Tree n eff ('Kind.Prim d)) = 'Kind.Tree n eff ('Kind.Prim (Maybe d))

-- type family ReplicateCon (qTrees :: [Kind.Tree]) (dCon :: Kind.Con) :: Kind.Con where
--   ReplicateCon qTrees ('Kind.Con name _) =
--     'Kind.Con name qTrees
--   ReplicateCon qTrees ('Kind.ConUna name _) =
--     'Kind.Con name qTrees

-- data ReplicateConF :: [Kind.Tree] -> Kind.Con -> Exp Kind.Con
-- type instance Eval (ReplicateConF qTrees con) = ReplicateCon qTrees con

-- type family ReplicateSum (qTree :: Kind.Tree) (dTrees :: [Kind.Con]) :: [Kind.Con] where
--   ReplicateSum _ '[] =
--     '[]
--   ReplicateSum ('Kind.Tree _ _ ('Kind.Prod _ qTrees)) dCons =
--     FMap (ReplicateConF qTrees) @@ dCons
--   ReplicateSum qTree ('Kind.Con name _ : dTrees) =
--     'Kind.ConUna name qTree : ReplicateSum qTree dTrees
--   ReplicateSum qTree ('Kind.ConUna name _ : dTrees) =
--     'Kind.ConUna name qTree : ReplicateSum qTree dTrees

-- type family SumPrimNames (q :: Type) (d :: Type) (cs :: [QCond]) :: [[Segment]] where
--   SumPrimNames _ _ '[] =
--     '[]
--   SumPrimNames q d ('SimpleCond q d n : cs) =
--     n : SumPrimNames q d cs
--   SumPrimNames q d ('SimpleCond q1 d1 n : _) =
--     TypeError (
--       "internal [SumPrimNames]:" %
--       "type mismatch between column types of query and data for prim/sum query:" %
--       "first constructor: " <> 'ShowType q <> " / " <> 'ShowType d %
--       "offending constructor: " <> 'ShowType q1 <> " / " <> 'ShowType d1 %
--       "path:" <> 'ShowType n
--     )
--   SumPrimNames _ _ cs =
--     '[ '[ErrorWithType "internal SumPrimNames: cannot group non-simple conds" cs]]

-- type family GroupSumPrim (simple :: QConds) :: QConds where
--   GroupSumPrim ('SimpleCond q d n : cs) =
--     '[ 'SumPrimCond q d (n : SumPrimNames q d cs)]
--   GroupSumPrim cs =
--     '[ErrorWithType "internal GroupSumPrim: cannot group non-simple conds" cs]

-- type family MatchPrim (prefix :: [Segment]) (name :: FieldId) (q :: Type) (d :: Type) :: QCond where
--   MatchPrim (p : ps) name q d =
--     'SimpleCond (ForceMaybe q) d (('FieldSegment name) : p : ps)
--   MatchPrim '[] name q d =
--     'SimpleCond q d '[('FieldSegment name)]
--   -- MatchPrim prefix name q d =
--   --   'SimpleCond q d ('FieldSegment name : prefix)

-- -- This silently ignores missing query fields because it's ok not to match all constructors.
-- -- Could be improved by doing a FirstJust kind of deal
-- type family MatchCon (meta :: QueryMeta) (prefix :: [Segment]) (qTree :: Kind.Con) (dTree :: Kind.Con) :: QConds where
--   MatchCon meta prefix ('Kind.Con _ q) ('Kind.Con conName d) =
--     FoldMap (MatchQueryColumnE meta ('ConSegment conName : prefix) d) @@ q
--   MatchCon meta prefix ('Kind.ConUna _ q) ('Kind.ConUna _ d) =
--     MatchQueryColumnE meta prefix '[d] @@ (ForceMaybePrim q)
--   MatchCon meta prefix ('Kind.ConUna _ q) ('Kind.Con conName d) =
--     MatchQueryColumnE meta ('ConSegment conName : prefix) d @@ ForceMaybePrim q
--   MatchCon meta prefix ('Kind.Con _ q) ('Kind.ConUna _ d) =
--     FoldMap (MatchQueryColumnE meta prefix '[d]) @@ q
--   MatchCon _ _ _ _ =
--     '[]

-- type family CheckCon (properSum :: Bool) (qCon :: Kind.Con) (conds :: QConds) :: QConds where
--   CheckCon 'False _ conds =
--     conds
--   CheckCon 'True ('Kind.Con _ '[]) '[] =
--     '[]
--   CheckCon 'True ('Kind.Con name _) '[] =
--     TypeError ("Query fields for constructor `" <> Eval (FieldIdSymbol name) <> "` didn't match any data fields")
--   CheckCon 'True _ conds =
--     conds

-- type family MatchCons (properSum :: Bool) (meta :: QueryMeta) (prefix :: [Segment]) (qTrees :: [Kind.Con]) (dTrees :: [Kind.Con]) :: [QConds] where
--   MatchCons _ _ _ '[] _ = '[]
--   MatchCons proper meta prefix (qCon : qCons) (dCon : dCons) =
--     CheckCon proper qCon (MatchCon meta prefix qCon dCon) : MatchCons proper meta prefix qCons dCons

-- type family MatchSum (meta :: QueryMeta) (prefix :: [Segment]) (qTrees :: [Kind.Con]) (dTrees :: [Kind.Con]) :: QConds where
--   MatchSum meta prefix qTrees dTrees =
--     MatchPrim prefix ('NamedField "sum__index") Int Int : Concat @@ MatchCons 'True meta prefix qTrees dTrees

-- type family MatchProd (meta :: QueryMeta) (prefix :: [Segment]) (flatten :: Bool) (qTree :: Kind.Tree) (dTree :: Kind.Tree) :: Maybe QConds where
--   MatchProd meta prefix 'True q ('Kind.Tree _ _ ('Kind.Prod  _ cols)) =
--     MatchCols meta prefix q cols
--   MatchProd meta prefix 'False ('Kind.Tree qname eff ('Kind.Prim q)) ('Kind.Tree name _ ('Kind.Prod _ cols)) =
--     MatchCols meta ('FieldSegment name : prefix) ('Kind.Tree qname eff ('Kind.Prim q)) cols
--   MatchProd _ _ _ _ _ =
--     'Nothing

-- type family UnwrapNewtype (a :: Type) (effs :: [Type]) :: Type where
--   UnwrapNewtype a '[] =
--     a
--   UnwrapNewtype a (Newtype a d : effs) =
--     UnwrapNewtype d effs
--   UnwrapNewtype a (_ : effs) =
--     UnwrapNewtype a effs

-- type family NodeTypeDesc (node :: Kind.Node) :: Symbol where
--   NodeTypeDesc ('Kind.Prim _) = "primitive"
--   NodeTypeDesc ('Kind.Prod _ _) = "product"
--   NodeTypeDesc ('Kind.SumProd _ _) = "sum"
--   NodeTypeDesc ('Kind.Sum _ _) = "sum"

-- type family MatchNode (meta :: QueryMeta) (prefix :: [Segment]) (name :: FieldId) (qTree :: Kind.Tree) (dTree :: Kind.Tree) :: Maybe QConds where
--   MatchNode _ prefix name ('Kind.Tree _ qEffs ('Kind.Prim q)) ('Kind.Tree _ dEffs ('Kind.Prim d)) =
--     'Just '[MatchPrim prefix name (UnwrapNewtype q qEffs) (UnwrapNewtype d dEffs)]
--   MatchNode meta prefix name ('Kind.Tree _ _ ('Kind.SumProd _ qTrees)) ('Kind.Tree _ _ ('Kind.SumProd _ dTrees)) =
--     'Just (MatchSum meta ('SumSegment name : prefix) qTrees dTrees)
--   MatchNode meta prefix name ('Kind.Tree qn qe ('Kind.Prod qt qTrees)) ('Kind.Tree _ _ ('Kind.SumProd _ dTrees)) =
--     'Just (Concat @@ MatchCons 'False meta ('SumSegment name : prefix) (ReplicateSum ('Kind.Tree qn qe ('Kind.Prod qt qTrees)) dTrees) dTrees)
--   MatchNode _ _ ('NamedField name) ('Kind.Tree _ _ qn) ('Kind.Tree _ _ n) =
--     'Just (TypeError (
--       "Incompatible column kinds for field `" <> name <> "` of type " <> 'ShowType (NodeDataType n) %
--       "Query is a " <> NodeTypeDesc qn %
--       "Data is a " <> NodeTypeDesc n
--     ))

-- type family MatchColWithUnderscore (meta :: QueryMeta) (prefix :: [Segment]) (qname :: FieldId) (dname :: FieldId) (dname_ :: Symbol) (qTree :: Kind.Tree) (dTree :: Kind.Tree) :: Maybe QConds where
--   MatchColWithUnderscore meta prefix qname qname _ qTree dTree =
--     MatchNode meta prefix qname qTree dTree
--   MatchColWithUnderscore meta prefix qname ('NamedField dname) dname qTree dTree =
--     MatchNode meta prefix qname qTree dTree
--   MatchColWithUnderscore _ _ _ _ _ _ _ =
--     'Nothing

-- -- |Match a data column against a query column.
-- -- This decides primarily based upon equality of field names, but it has to compensate for underscore prefixes, which it
-- -- does by passing an additional argument to 'MatchNode', the query field name prefixed with underscore.
-- -- Since product types cannot directly match, the first equation delegates them directly to 'MatchProd', ignoreing the
-- -- column names.
-- type family MatchCol' (meta :: QueryMeta) (prefix :: [Segment]) (qname :: FieldId) (dname :: FieldId) (qTree :: Kind.Tree) (dTree :: Kind.Tree) :: Maybe QConds where
--   MatchCol' meta prefix _ _ q ('Kind.Tree name effs ('Kind.Prod d cols)) =
--     MatchProd meta prefix (ContainsFlatten effs) q ('Kind.Tree name effs ('Kind.Prod d cols))
--   MatchCol' meta prefix qname qname qTree dTree =
--     MatchNode meta prefix qname qTree dTree
--   MatchCol' meta prefix ('NamedField qname) ('NamedField dname) qTree dTree =
--     MatchColWithUnderscore meta prefix ('NamedField qname) ('NamedField dname) (AppendSymbol "_" qname) qTree dTree
--   MatchCol' _ _ _ _ _ _ =
--     'Nothing

-- data MatchCol (meta :: QueryMeta) (prefix :: [Segment]) (qTree :: Kind.Tree) :: Kind.Tree -> Exp (Maybe QConds)
-- type instance Eval (MatchCol meta prefix ('Kind.Tree qname qEff qTrees) ('Kind.Tree dname dEff dTrees)) =
--   MatchCol' meta prefix qname dname ('Kind.Tree qname qEff qTrees) ('Kind.Tree dname dEff dTrees)

-- type family MatchCols (meta :: QueryMeta) (prefix :: [Segment]) (qTree :: Kind.Tree) (dTrees :: [Kind.Tree]) :: Maybe QConds where
--   MatchCols meta prefix qTree dTrees =
--     FirstJust (MatchCol meta prefix qTree) @@ dTrees

-- data MatchQueryColumnE (meta :: QueryMeta) (prefix :: [Segment]) :: [Kind.Tree] -> Kind.Tree -> Exp QConds
-- type instance Eval (MatchQueryColumnE meta prefix dTrees qTree) =
--   FromMaybe (MissingColumn meta qTree) @@ MatchCols meta prefix qTree dTrees

-- type family MatchTable (meta :: QueryMeta) (qTree :: Kind.Tree) (dTree :: Kind.Tree) :: QConds where
--   MatchTable meta ('Kind.Tree _ _ ('Kind.Prod _ qTrees)) ('Kind.Tree _ _ ('Kind.Prod _ dTrees)) =
--     FoldMap (MatchQueryColumnE meta '[] dTrees) @@ qTrees
--   MatchTable meta ('Kind.Tree n e ('Kind.Prim t)) ('Kind.Tree _ _ ('Kind.Prod _ dTrees)) =
--     MatchQueryColumnE meta '[] dTrees @@ ('Kind.Tree n e ('Kind.Prim t))
--   MatchTable meta ('Kind.Tree _ _ ('Kind.Sum _ qTrees)) ('Kind.Tree _ _ ('Kind.Sum _ dCons)) =
--     MatchSum meta '[] qTrees dCons
--   MatchTable meta ('Kind.Tree _ _ ('Kind.SumProd _ qCons)) ('Kind.Tree _ _ ('Kind.SumProd _ dCons)) =
--     MatchSum meta '[] qCons dCons
--   MatchTable meta ('Kind.Tree qn e ('Kind.Prim t)) ('Kind.Tree _ _ ('Kind.SumProd _ dCons)) =
--     GroupSumPrim (Concat @@ MatchCons 'False meta '[] (ReplicateSum ('Kind.Tree qn e ('Kind.Prim t)) dCons) dCons)
--   MatchTable _ qTree dTree =
--     ErrorWithType "MatchTable" '(qTree, dTree)
