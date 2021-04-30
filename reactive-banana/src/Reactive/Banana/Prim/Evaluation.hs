{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Reactive.Banana.Prim.Evaluation
  ( step,
  )
where

import Control.Monad (join)
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.RWSIO as RWS
import Data.Foldable (traverse_)
import Data.List (foldl')
import qualified Data.PQueue.Prio.Min as Q
import Data.Vault.Lazy (Vault)
import qualified Reactive.Banana.Prim.OrderedBag as OB
import Reactive.Banana.Prim.Plumbing
import Reactive.Banana.Prim.Types
import Reactive.Banana.Type.Ref
import System.Mem.Weak

type Queue = Q.MinPQueue Level

{-----------------------------------------------------------------------------
    Evaluation step
------------------------------------------------------------------------------}

-- | Evaluate all the pulses in the graph,
-- Rebuild the graph as necessary and update the latch values.
step :: ([SomeNode], Vault) -> Network -> IO (IO (), Network)
step (inputs, pulses) Network {nTime = time1, nOutputs = outputs1, nAlwaysP} =
  -- we assume that this has been built already
  case nAlwaysP of
    Nothing -> undefined
    Just alwaysPulse -> do
      -- evaluate pulses
      ((_, (latchUpdates, outputs)), topologyUpdates, os) <-
        runBuildIO (time1, alwaysPulse) $
          runEvalP pulses $
            evaluatePulses inputs

      -- update latch values from pulses
      latchUpdates

      -- rearrange graph topology
      topologyUpdates

      let actions :: [(Output, EvalO)]
          actions = OB.inOrder outputs outputs1 -- EvalO actions in proper order
          state2 :: Network
          state2 =
            Network
              { nTime = next time1,
                nOutputs = foldl' OB.insert outputs1 os,
                nAlwaysP = Just alwaysPulse
              }

      pure (runEvalOs $ map snd actions, state2)

runEvalOs :: [EvalO] -> IO ()
runEvalOs = traverse_ join

{-----------------------------------------------------------------------------
    Traversal in dependency order
------------------------------------------------------------------------------}

-- | Update all pulses in the graph, starting from a given set of nodes
evaluatePulses :: [SomeNode] -> EvalP ()
evaluatePulses roots = RWS.R $ \r -> go r =<< insertNodes r roots Q.empty
  where
    go :: RWS.Tuple BuildR (EvalPW, BuildW) Vault -> Queue SomeNode -> IO ()
    go r q0 =
      case Q.minView q0 of
        Nothing -> return ()
        Just (node, q1) -> do
          children <- unwrapEvalP r (evaluateNode node)
          q2 <- insertNodes r children q1
          go r q2

-- | Recalculate a given node and return all children nodes
-- that need to evaluated subsequently.
evaluateNode :: SomeNode -> EvalP [SomeNode]
evaluateNode (P p) =
  do
    Pulse {..} <- liftIO (readRef p)
    ma <- _evalP
    writePulseP _keyP ma
    case ma of
      Nothing -> return []
      Just _ -> liftIO $ deRefWeaks _childrenP
evaluateNode (L lw) = do
  time <- askTime
  LatchWrite {..} <- liftIO (readRef lw)
  mlatch <- liftIO $ deRefWeak _latchLW -- retrieve destination latch
  case mlatch of
    Nothing -> return ()
    Just latch -> do
      a <- _evalLW -- calculate new latch value
      -- liftIO $ Strict.evaluate a      -- see Note [LatchStrictness]
      rememberLatchUpdate $ -- schedule value to be set later
        modifyRef latch \l ->
          a `seq` l {_seenL = time, _valueL = a}
  return []
evaluateNode (O o) = do
  Output {..} <- liftIO (readRef o)
  m <- _evalO -- calculate output action
  rememberOutput (o, m)
  return []

-- | Insert nodes into the queue
insertNodes :: RWS.Tuple BuildR (EvalPW, BuildW) Vault -> [SomeNode] -> Queue SomeNode -> IO (Queue SomeNode)
insertNodes (RWS.Tuple (time, _) _ _) = go
  where
    go :: [SomeNode] -> Queue SomeNode -> IO (Queue SomeNode)
    go [] q = return q
    go (node@(P p) : xs) q = do
      pulse@Pulse {_levelP, _seenP} <- readRef p
      if time <= _seenP
        then go xs q -- pulse has already been put into the queue once
        else do
          -- pulse needs to be scheduled for evaluation
          writeRef p $! pulse {_seenP = time}
          go xs (Q.insert _levelP node q)
    go (node : xs) q = go xs (Q.insert ground node q)

-- O and L nodes have only one parent, so
-- we can insert them at an arbitrary level
