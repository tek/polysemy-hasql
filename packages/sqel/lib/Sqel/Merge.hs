module Sqel.Merge where

import Sqel.Data.Dd (CompInc (Merge), Dd (Dd), DdInc (DdMerge), DdK (DdK), DdStruct (DdComp), Struct (Comp))

class Merge a s0 s1 | a s0 -> s1 where
  merge :: Dd s0 -> Dd s1

instance Merge a ('DdK sel mods b ('Comp tsel c i s)) ('DdK sel mods b ('Comp tsel c 'Merge s)) where
  merge (Dd sel mods (DdComp tsel c _ sub)) =
    Dd sel mods (DdComp tsel c DdMerge sub)
