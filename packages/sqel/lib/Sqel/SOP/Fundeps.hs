{-# language UndecidableSuperClasses #-}

module Sqel.SOP.Fundeps where

import Generics.SOP (AllZip)

class c a b => Fundep (c :: k -> l -> Constraint) (a :: k) (b :: l) | c a -> b where

type family UnFundep (c :: a -> b -> Constraint) :: (a -> b -> Constraint) where
  UnFundep (Fundep c) = c

class AllZip (UnFundep c) as bs => Fundeps (c :: k -> l -> Constraint) (as :: [k]) (bs :: [l]) | c as -> bs where

instance Fundeps (Fundep c) '[] '[] where

instance (
    Fundep c a b,
    Fundeps (Fundep c) as bs
  ) => Fundeps (Fundep c) (a : as) (b : bs) where

class DummyDep arg s | arg -> s where
