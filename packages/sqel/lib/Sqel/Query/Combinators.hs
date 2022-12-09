module Sqel.Query.Combinators where

import Sqel.Data.Dd (Dd (Dd), DdK (DdK), Struct (Prim))
import Sqel.Data.Sel (Sel (SelUnused), mkSel)
import Sqel.Prim (primMod)
import Sqel.Sql.Prepared (dollar)
import Sqel.Sql.Select (FragType (Limit, Offset), SelectAtom (SelectAtom))

nocond :: Dd ('DdK sel p a s) -> Dd ('DdK 'SelUnused p a s)
nocond (Dd _ p s) =
  Dd mkSel p s

limit ::
  Dd ('DdK 'SelUnused '[SelectAtom] Int64 'Prim)
limit =
  nocond (primMod (SelectAtom Limit (const dollar)))

offset ::
  Dd ('DdK 'SelUnused '[SelectAtom] Int64 'Prim)
offset =
  nocond (primMod (SelectAtom Offset (const dollar)))
