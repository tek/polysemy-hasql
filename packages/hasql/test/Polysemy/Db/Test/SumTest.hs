{-# LANGUAGE UndecidableSuperClasses #-}

module Polysemy.Db.Test.SumTest where

import Data.Functor.Contravariant.Divisible (Decidable, Divisible, choose)
import Generics.SOP (from, unSOP, hindex, hcpure, (:.:)(Comp), hpure, All, All2, Code, Generic, I(I), NP(Nil, (:*)), NS(Z, S), Top)
import Polysemy.Test (UnitTest)
import Prelude hiding (All, Generic)

import Polysemy.Hasql.Table.QueryParams (sequenceContravariantNP, sequenceContravariantNPF)

data Dat =
  D1 { foo :: Int, foo1 :: Double, foo2 :: Text }
  |
  D2 { bar :: Text, zoo :: Double }
  deriving (Eq, Show)

deriveGeneric ''Dat

newtype Contr b =
  Contr { unContr :: Op Text b }
  deriving newtype (Semigroup, Monoid, Contravariant, Divisible, Decidable)

contr :: (b -> Text) -> Contr b
contr =
  Contr . Op

runContr :: Contr a -> a -> Text
runContr =
  getOp . unContr

dat :: Dat
dat =
  D2 "bear" 5.5

class MkContr a where
  mkContr :: Contr a

instance Show a => MkContr a where
  mkContr =
    contr show

class All Top a => MkContrs (a :: [*]) where
  mkContrs :: NP Contr a

instance MkContrs '[] where
  mkContrs =
    Nil

instance (
    MkContr d,
    MkContrs ds
  ) => MkContrs (d : ds) where
    mkContrs =
      mkContr @d :* mkContrs @ds

unconsNS ::
  NS (NP I) (as : ass) ->
  Either (NP I as) (NS (NP I) ass)
unconsNS = \case
  Z x -> Left x
  S x -> Right x

nulls ::
  ∀ (as :: [*]) (ass :: [[*]]) .
  All Top as =>
  All Show as =>
  Contr (NS (NP I) ass)
nulls =
  contramap (const nls) row
  where
    nls :: NP (I :.: Maybe) as
    nls =
      hpure (Comp (I Nothing))
    row :: Contr (NP (I :.: Maybe) as)
    row =
      sequenceContravariantNPF (hcpure (Proxy @Show) (Comp nl))
    nl :: ∀ a . Show a => Contr (Maybe a)
    nl =
      choose (maybeToRight ()) (contr (const "<null>")) (mkContr @a)

class Choose (ass :: [[*]]) where
  choose' :: Contr (NS (NP I) ass)

instance Choose '[] where
  choose' =
    mempty

instance (
  All Show as,
  Choose ass,
  MkContrs as
  ) => Choose (as : ass) where
  choose' =
    choose unconsNS (sequenceContravariantNP (mkContrs @as)) (nulls @as <> choose')

int :: Contr Int
int =
  contr show

sumIndex ::
  Contr (NS (NP I) ass)
sumIndex =
  contramap hindex int

withChoose ::
  ∀ (a :: *) (as :: [*]) (ass :: [[*]]) .
  All Show as =>
  Generic a =>
  Code a ~ (as : ass) =>
  All2 Top (as : ass) =>
  MkContrs as =>
  Choose ass =>
  Contr a
withChoose =
  contramap (unSOP . from) (sumIndex <> choose')

test_sum :: UnitTest
test_sum = do
  dbgs (runContr withChoose dat)
