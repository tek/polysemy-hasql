{-# LANGUAGE NoImplicitPrelude #-}

module Polysemy.Db.Prelude (
  module Polysemy.Db.Prelude,
  module Control.Lens,
  module Data.Aeson,
  module Data.Aeson.TH,
  module Data.Composition,
  module Data.Default,
  module Data.Either.Combinators,
  module Data.Foldable,
  module Data.List.NonEmpty,
  module Data.Map.Strict,
  module Data.UUID,
  module Debug.Trace,
  module GHC.Err,
  module GHC.TypeLits,
  module Generics.SOP.TH,
  module Polysemy,
  module Polysemy.AtomicState,
  module Polysemy.Db.Debug,
  module Polysemy.Error,
  module Polysemy.Internal.Bundle,
  module Polysemy.Reader,
  module Polysemy.State,
  module Relude,
) where

import Control.Exception (throwIO, try)
import Control.Lens (at, makeClassy, over, (%~), (.~), (<>~), (?~), (^.))
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.Aeson.TH (deriveFromJSON, deriveJSON)
import Data.Composition ((.:), (.:.), (.::))
import Data.Default (Default(def))
import Data.Either.Combinators (mapLeft)
import Data.Foldable (foldl, traverse_)
import Data.List.NonEmpty ((<|))
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, lookup)
import Data.String.Interpolate (i)
import qualified Data.Text as Text
import Data.UUID (UUID)
import Debug.Trace (trace, traceShow)
import GHC.Err (undefined)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.TypeLits (Symbol)
import Generics.SOP.TH (deriveGeneric)
import Language.Haskell.TH.Quote (QuasiQuoter)
import qualified Language.Haskell.TH.Syntax as TH
import Polysemy (
  Effect,
  EffectRow,
  Embed,
  Final,
  InterpreterFor,
  Member,
  Members,
  Sem,
  WithTactics,
  embed,
  embedToFinal,
  interpret,
  makeSem,
  pureT,
  raise,
  raiseUnder,
  raiseUnder2,
  raiseUnder3,
  reinterpret,
  runFinal,
  )
import Polysemy.AtomicState (AtomicState, atomicGet, atomicGets, atomicModify', atomicPut, runAtomicStateTVar)
import Polysemy.Db.Debug (dbg, dbgs, dbgs_)
import Polysemy.Error (Error, fromEither, mapError, note, runError, throw)
import Polysemy.Internal.Bundle (Append)
import Polysemy.Reader (Reader)
import Polysemy.State (State, evalState, get, gets, modify, modify', put, runState)
import Relude hiding (
  Reader,
  State,
  Sum,
  Type,
  ask,
  asks,
  evalState,
  filterM,
  get,
  gets,
  hoistEither,
  modify,
  modify',
  put,
  readFile,
  runReader,
  runState,
  state,
  trace,
  traceShow,
  undefined,
  )
import System.IO.Error (userError)

unit ::
  Applicative f =>
  f ()
unit =
  pure ()
{-# inline unit #-}

tuple ::
  Applicative f =>
  f a ->
  f b ->
  f (a, b)
tuple fa fb =
  (,) <$> fa <*> fb
{-# inline tuple #-}

unsafeLogSAnd :: Show a => a -> b -> b
unsafeLogSAnd a b =
  unsafePerformIO $ print a >> return b
{-# inline unsafeLogSAnd #-}

unsafeLogAnd :: Text -> b -> b
unsafeLogAnd a b =
  unsafePerformIO $ putStrLn (toString a) >> return b
{-# inline unsafeLogAnd #-}

unsafeLogS :: Show a => a -> a
unsafeLogS a =
  unsafePerformIO $ print a >> return a
{-# inline unsafeLogS #-}

unsafeLog :: Text -> Text
unsafeLog a =
  unsafePerformIO $ putStrLn (toString a) >> return a
{-# inline unsafeLog #-}

liftT ::
  forall m f r e a .
  Functor f =>
  Sem r a ->
  Sem (WithTactics e f m r) (f a)
liftT =
  pureT <=< raise
{-# inline liftT #-}

hoistEither ::
  Member (Error e2) r =>
  (e1 -> e2) ->
  Either e1 a ->
  Sem r a
hoistEither f =
  fromEither . mapLeft f
{-# inline hoistEither #-}

hoistEitherWith ::
  (e -> Sem r a) ->
  Either e a ->
  Sem r a
hoistEitherWith f =
  either f pure
{-# inline hoistEitherWith #-}

hoistEitherShow ::
  Show e1 =>
  Member (Error e2) r =>
  (Text -> e2) ->
  Either e1 a ->
  Sem r a
hoistEitherShow f =
  fromEither . mapLeft (f . Text.replace "\\" "" . show)
{-# inline hoistEitherShow #-}

hoistErrorWith ::
  (e -> Sem r a) ->
  Sem (Error e : r) a ->
  Sem r a
hoistErrorWith f =
  hoistEitherWith f <=< runError
{-# inline hoistErrorWith #-}

tryAny ::
  Member (Embed IO) r =>
  IO a ->
  Sem r (Either Text a)
tryAny =
  embed . fmap (mapLeft show) . try @SomeException
{-# inline tryAny #-}

tryHoist ::
  Member (Embed IO) r =>
  (Text -> e) ->
  IO a ->
  Sem r (Either e a)
tryHoist f =
  fmap (mapLeft f) . tryAny
{-# inline tryHoist #-}

tryThrow ::
  Members [Embed IO, Error e] r =>
  (Text -> e) ->
  IO a ->
  Sem r a
tryThrow f =
  fromEither <=< tryHoist f
{-# inline tryThrow #-}

throwTextIO :: Text -> IO a
throwTextIO =
  throwIO . userError . toString
{-# inline throwTextIO #-}

throwEitherIO :: Either Text a -> IO a
throwEitherIO =
  traverseLeft throwTextIO
{-# inline throwEitherIO #-}

basicOptions :: Aeson.Options
basicOptions =
  Aeson.defaultOptions {
    Aeson.fieldLabelModifier = dropWhile ('_' ==)
  }

jsonOptions :: Aeson.Options
jsonOptions =
  basicOptions {
    Aeson.unwrapUnaryRecords = True
  }

defaultJson :: TH.Name -> TH.Q [TH.Dec]
defaultJson =
  deriveJSON jsonOptions
{-# inline defaultJson #-}

unaryRecordJson :: TH.Name -> TH.Q [TH.Dec]
unaryRecordJson =
  deriveJSON basicOptions
{-# inline unaryRecordJson #-}

type Basic a =
  (Eq a, Show a)

type family Basics (as :: [*]) :: Constraint where
  Basics '[] = ()
  Basics (a : as) = (Basic a, Basics as)

type Eso a =
  (Basic a, Ord a)

type family Esos (as :: [*]) :: Constraint where
  Esos '[] = ()
  Esos (a : as) = (Eso a, Esos as)

type Json a =
  (FromJSON a, ToJSON a, Basic a)

type family Jsons (r :: [*]) :: Constraint where
  Jsons '[] = ()
  Jsons (a ': r) = (Json a, Jsons r)

type a ++ b =
  Append a b

rightOr :: (a -> b) -> Either a b -> b
rightOr f =
  either f id
{-# inline rightOr #-}

traverseLeft ::
  Applicative m =>
  (a -> m b) ->
  Either a b ->
  m b
traverseLeft f =
  either f pure
{-# inline traverseLeft #-}

jsonDecode ::
  FromJSON a =>
  ByteString ->
  Either Text a
jsonDecode =
  mapLeft toText . Aeson.eitherDecodeStrict'
{-# inline jsonDecode #-}

jsonDecodeL ::
  FromJSON a =>
  LByteString ->
  Either Text a
jsonDecodeL =
  mapLeft toText . Aeson.eitherDecode'
{-# inline jsonDecodeL #-}

jsonDecodeText ::
  FromJSON a =>
  Text ->
  Either Text a
jsonDecodeText =
  mapLeft toText . Aeson.eitherDecodeStrict' . encodeUtf8
{-# inline jsonDecodeText #-}

jsonEncode ::
  ToJSON a =>
  a ->
  ByteString
jsonEncode =
  toStrict . Aeson.encode
{-# inline jsonEncode #-}

jsonEncodeText ::
  ToJSON a =>
  a ->
  Text
jsonEncodeText =
  decodeUtf8 . jsonEncode
{-# inline jsonEncodeText #-}

as ::
  Functor m =>
  a ->
  m b ->
  m a
as =
  (<$)
{-# inline as #-}

mneToList :: Maybe (NonEmpty a) -> [a]
mneToList =
  maybe [] toList
{-# inline mneToList #-}

qt :: QuasiQuoter
qt =
  i
{-# inline qt #-}

safeDiv ::
  Eq a =>
  Fractional a =>
  a ->
  a ->
  Maybe a
safeDiv _ 0 =
  Nothing
safeDiv n d =
  Just (n / d)
{-# inline safeDiv #-}

divOr0 ::
  Eq a =>
  Fractional a =>
  a ->
  a ->
  a
divOr0 =
  fromMaybe 0 .: safeDiv
{-# inline divOr0 #-}

mapBy ::
  Ord k =>
  (a -> k) ->
  [a] ->
  Map k a
mapBy f =
  Map.fromList . fmap \ a -> (f a, a)
