module Sqel.Query.SelectExpr where

import Generics.SOP (All, K (K), NP ((:*)), hcmap, hcollapse)

import Sqel.Mods (defaultWhere)
import Sqel.Comp (IndexColumn)
import Sqel.Data.Mods (Mods, GetMod (getMod))
import Sqel.Data.Dd (
  Comp (Prod, Sum),
  CompInc (Merge, Nest),
  Dd (Dd),
  DdInc (DdMerge, DdNest),
  DdK (DdK),
  DdStruct (DdComp, DdPrim),
  DdVar (DdSum),
  QOp (QAnd),
  Struct (Comp, Prim),
  )
import Sqel.Data.Sel (Sel (SelSymbol, SelUnused), SelW (SelWAuto))
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql (Sql), sql)
import Sqel.Query.Fragments (ColumnPrefix, QFragmentPrefix (qfragmentPrefix), prefixed)
import Sqel.Sql.Prepared (dollar)
import Sqel.Sql.Select (
  FragType (Where),
  SelectAtom (SelectAtom),
  SelectExpr (SelectExprAtom, SelectExprList, SelectExprSum),
  )
import Sqel.Text.DbIdentifier (dbSymbol)

guardSum :: SelectExpr -> SelectExpr
guardSum = \case
  SelectExprAtom Where code -> SelectExprAtom Where \ i -> [sql|(#{dollar i} is null or #{code i})|]
  SelectExprList op sub -> SelectExprList op (guardSum <$> sub)
  expr -> expr

class ToSelectExpr query where
  toSelectExpr :: ColumnPrefix -> Dd query -> SelectExpr

-- TODO this creates an invalid fragment, but it seems not to be used
instance (
    GetMod () SelectAtom ps
  ) => ToSelectExpr ('DdK 'SelUnused (Mods ps) q 'Prim) where
  toSelectExpr _ (Dd _ p DdPrim) =
    SelectExprAtom type_ (code "")
    where
      SelectAtom type_ code = getMod @() defaultWhere p

instance (
    KnownSymbol n,
    GetMod () SelectAtom ps
  ) => ToSelectExpr ('DdK ('SelSymbol n) (Mods ps) q 'Prim) where
  toSelectExpr pre (Dd _ p DdPrim) =
    SelectExprAtom type_ (code (Selector (Sql (prefixed (dbSymbol @n) pre))))
    where
      SelectAtom type_ code = getMod @() defaultWhere p

prodSelectExpr ::
  ∀ sel s .
  All ToSelectExpr s =>
  QFragmentPrefix sel =>
  SelW sel ->
  ColumnPrefix ->
  QOp ->
  NP Dd s ->
  SelectExpr
prodSelectExpr sel pre op =
  SelectExprList op . hcollapse . hcmap (Proxy @ToSelectExpr) (K . toSelectExpr (qfragmentPrefix sel pre))

sumSelectExpr ::
  ∀ sel s .
  All ToSelectExpr s =>
  QFragmentPrefix sel =>
  SelW sel ->
  ColumnPrefix ->
  NP Dd s ->
  SelectExpr
sumSelectExpr sel pre =
  SelectExprSum . hcollapse . hcmap (Proxy @ToSelectExpr) (K . toSelectExpr (qfragmentPrefix sel pre))

-- TODO add QOp param lookup
instance (
    All ToSelectExpr sub,
    QFragmentPrefix sel
  ) => ToSelectExpr ('DdK sel p q ('Comp tsel ('Prod con) 'Nest sub)) where
  toSelectExpr pre = \case
    Dd sel _ (DdComp _ _ DdNest sub) ->
      prodSelectExpr sel pre QAnd sub

instance (
    All ToSelectExpr sub
  ) => ToSelectExpr ('DdK sel p q ('Comp tsel ('Prod con) 'Merge sub)) where
  toSelectExpr pre = \case
    Dd _ _ (DdComp _ _ DdMerge sub) ->
      prodSelectExpr SelWAuto pre QAnd sub

instance (
    All ToSelectExpr sub,
    QFragmentPrefix sel
  ) => ToSelectExpr ('DdK sel p q ('Comp tsel 'Sum 'Nest (IndexColumn name : sub))) where
  toSelectExpr pre = \case
    Dd sel _ (DdComp _ DdSum DdNest (_ :* sub)) ->
      guardSum (sumSelectExpr sel pre sub)
