{-# LANGUAGE BangPatterns #-}

module Reactive.Banana.Prim.Compile
  ( compile,
    mapAccumM,
    mapAccumM_,
  )
where

import Data.List (foldl')
import Reactive.Banana.Prim.Plumbing
import Reactive.Banana.Prim.Types
import qualified Reactive.Banana.Type.OSet as OSet
import Reactive.Banana.Type.Time (beginning, next)

{-----------------------------------------------------------------------------
   Compilation
------------------------------------------------------------------------------}

-- | Change a 'Network' of pulses and latches by executing a 'BuildIO' action.
compile :: Build () -> IO Network
compile m = do
  nAlwaysP <- newPulse "alwaysP" (pure (Just ()))

  let time1 = next beginning

  ((), topology, os) <- runBuildIO (time1, nAlwaysP) m
  topology

  pure
    Network
      { nTime = next time1,
        nOutputs = foldl' OSet.insert OSet.empty os,
        nAlwaysP
      }

{-----------------------------------------------------------------------------
    Testing
------------------------------------------------------------------------------}

-- | 'mapAccum' for a monad.
mapAccumM :: Monad m => (a -> s -> m (b, s)) -> s -> [a] -> m [b]
mapAccumM _ _ [] = return []
mapAccumM f s0 (x : xs) = do
  (b, s1) <- f x s0
  bs <- mapAccumM f s1 xs
  return (b : bs)

-- | Strict 'mapAccum' for a monad. Discards results.
mapAccumM_ :: Monad m => (a -> s -> m (b, s)) -> s -> [a] -> m ()
mapAccumM_ _ _ [] = return ()
mapAccumM_ f !s0 (x : xs) = do
  (_, s1) <- f x s0
  mapAccumM_ f s1 xs
