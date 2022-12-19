module Sqel.Query.Combinators where

import Sqel.Class.Mods (MapMod, setMod)
import Sqel.Data.Dd (Dd (Dd), DdK (DdK), Struct (Prim))
import Sqel.Data.FragType (FragType (Limit, Offset, Order, Where))
import Sqel.Data.Order (Order)
import Sqel.Data.Sel (Sel (SelAuto, SelUnused), mkSel)
import Sqel.Data.Select (SelectAtom (SelectAtom))
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Data.Sql (Sql, sql)
import Sqel.Prim (primMod)
import Sqel.Sql.Prepared (dollar)

whereOp :: Sql -> SelectAtom
whereOp op =
  SelectAtom Where (\ sel i -> [sql|##{sel} #{op} #{dollar i}|])

whereEq :: SelectAtom
whereEq =
  whereOp "="

whereGt :: SelectAtom
whereGt =
  whereOp ">"

whereGte :: SelectAtom
whereGte =
  whereOp ">="

whereLt :: SelectAtom
whereLt =
  whereOp "<"

whereLte :: SelectAtom
whereLte =
  whereOp "<="

whereLike :: SelectAtom
whereLike =
  whereOp "like"

greater ::
  MapMod SelectAtom s0 s1 =>
  Dd s0 ->
  Dd s1
greater =
  setMod whereGt

greaterEq ::
  MapMod SelectAtom s0 s1 =>
  Dd s0 ->
  Dd s1
greaterEq =
  setMod whereGte

less ::
  MapMod SelectAtom s0 s1 =>
  Dd s0 ->
  Dd s1
less =
  setMod whereLt

lessEq ::
  MapMod SelectAtom s0 s1 =>
  Dd s0 ->
  Dd s1
lessEq =
  setMod whereLte

like ::
  MapMod SelectAtom s0 s1 =>
  Dd s0 ->
  Dd s1
like =
  setMod whereLike

nocond :: Dd ('DdK sel p a s) -> Dd ('DdK 'SelUnused p a s)
nocond (Dd _ p s) =
  Dd mkSel p s

limit ::
  Dd ('DdK 'SelUnused '[SelectAtom] a 'Prim)
limit =
  nocond (primMod (SelectAtom Limit (const dollar)))

offset ::
  Dd ('DdK 'SelUnused '[SelectAtom] a 'Prim)
offset =
  nocond (primMod (SelectAtom Offset (const dollar)))

order ::
  Order ->
  Dd ('DdK 'SelAuto '[SelectAtom] a 'Prim)
order dir =
  primMod (SelectAtom (Order dir) (\ (Selector sel) _ -> sel))
