{-# language UndecidableSuperClasses #-}

module Polysemy.Db.SOP.Fundeps where

import Generics.SOP (AllZip, SListI, SameShapeAs)

class c a b => Fundep (c :: k -> l -> Constraint) (a :: k) (b :: l) | c a -> b where

type family UnFundep (c :: a -> b -> Constraint) :: (a -> b -> Constraint) where
  UnFundep (Fundep c) = c

class AllZip (UnFundep c) as bs => Fundeps (c :: k -> l -> Constraint) (as :: [k]) (bs :: [l]) | c as -> bs where

instance Fundeps (Fundep c) '[] '[] where

instance (
    Fundep c a b,
    Fundeps (Fundep c) as bs
  ) => Fundeps (Fundep c) (a : as) (b : bs) where

type family AllZipF2 (cls :: a -> b -> c -> Constraint) (as :: [a]) (bs :: [b]) (cs :: [d]) :: Constraint where
  AllZipF2 _ '[] '[] '[] = ()
  AllZipF2 cls (a : as) (b : bs) (c : cs) = (cls a b c, AllZip2 cls as bs cs)

class (
    SListI as,
    SListI bs,
    SameShapeAs as bs,
    SameShapeAs bs as,
    AllZipF2 cls as bs cs
  ) => AllZip2 (cls :: a -> b -> c -> Constraint) (as :: [a]) (bs :: [b]) (cs :: [c])

instance (
    SListI as,
    SListI bs,
    SameShapeAs as bs,
    SameShapeAs bs as,
    AllZipF2 cls as bs cs
  ) => AllZip2 cls as bs cs

class cls a b c => Fundep2 (cls :: k -> l -> m -> Constraint) (a :: k) (b :: l) (c :: m) | cls a b -> c where

type family UnFundep2 (cls :: a -> b -> c -> Constraint) :: (a -> b -> c -> Constraint) where
  UnFundep2 (Fundep2 c) = c

class (
    AllZip2 (UnFundep2 cls) as bs cs
  ) => Fundeps2 (cls :: k -> l -> m -> Constraint) (as :: [k]) (bs :: [l]) (cs :: [m]) | cls as bs -> cs where

instance Fundeps2 (Fundep2 cls) '[] '[] '[] where

instance (
    Fundep2 cls a b c,
    Fundeps2 (Fundep2 cls) as bs cs
  ) => Fundeps2 (Fundep2 cls) (a : as) (b : bs) (c : cs) where
