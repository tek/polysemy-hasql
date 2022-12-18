module Sqel.Query.Combinators where

import Sqel.Data.Dd (Dd (Dd), DdK (DdK), Struct (Prim))
import Sqel.Data.FragType (FragType (Limit, Offset, Order))
import Sqel.Data.Order (Order)
import Sqel.Data.Sel (Sel (SelAuto, SelUnused), mkSel)
import Sqel.Data.Select (SelectAtom (SelectAtom))
import Sqel.Data.Selector (Selector (Selector))
import Sqel.Prim (primMod)
import Sqel.Sql.Prepared (dollar)

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
