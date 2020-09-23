{-# LANGUAGE NoImplicitPrelude #-}

module Polysemy.Db.Debug where

import Data.String.Interpolate (i)
import GHC.Stack (SrcLoc(..))
import Relude
import System.IO.Unsafe (unsafePerformIO)

srcLoc :: CallStack -> SrcLoc
srcLoc = \case
  (getCallStack -> (_, loc) : _) -> loc
  _ -> error "Debug.srcLoc: empty CallStack"

debugPrintWithLoc ::
  Monad m =>
  SrcLoc ->
  Text ->
  m ()
debugPrintWithLoc SrcLoc{..} msg = do
  () <- return $ unsafePerformIO (putStrLn [i|#{srcLocModule}:#{srcLocStartLine}:#{srcLocStartCol} #{msg}|])
  pure ()

dbg ::
  HasCallStack =>
  Monad m =>
  Text ->
  m ()
dbg =
  debugPrintWithLoc (srcLoc callStack)
{-# inline dbg #-}

dbgs ::
  HasCallStack =>
  Monad m =>
  Show a =>
  a ->
  m ()
dbgs a =
  debugPrintWithLoc (srcLoc callStack) (show a)
{-# inline dbgs_ #-}

dbgs_ ::
  HasCallStack =>
  Monad m =>
  Show a =>
  a ->
  m a
dbgs_ a =
  a <$ debugPrintWithLoc (srcLoc callStack) (show a)
{-# inline dbgs #-}
