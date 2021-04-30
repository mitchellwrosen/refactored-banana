{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE NamedFieldPuns #-}
{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards #-}

module Reactive.Banana.Prim.Dependencies
  ( -- | Utilities for operating on node dependencies.
    addChild,
    changeParent,
    buildDependencies,
  )
where

import Control.Monad
import Data.Foldable (for_)
import Data.Monoid
import Reactive.Banana.Prim.Types
import Reactive.Banana.Type.Graph (Graph)
import qualified Reactive.Banana.Type.Graph as Graph
import Reactive.Banana.Type.Ref
import System.Mem.Weak

{-----------------------------------------------------------------------------
    Accumulate dependency information for nodes
------------------------------------------------------------------------------}

-- | Add a new child node to a parent node.
addChild :: SomeNode -> SomeNode -> DependencyBuilder
addChild parent child = (Endo $ Graph.insertEdge (parent, child), mempty)

-- | Assign a new parent to a child node.
-- INVARIANT: The child may have only one parent node.
changeParent :: PulseRef a -> PulseRef b -> DependencyBuilder
changeParent child parent = (mempty, [(P child, P parent)])

-- | Execute the information in the dependency builder
-- to change network topology.
buildDependencies :: DependencyBuilder -> IO ()
buildDependencies (Endo f, parents) = do
  sequence_ [x `doAddChild` y | (x, y) <- Graph.getEdges gr]
  sequence_ [x `doChangeParent` y | (P x, P y) <- parents]
  where
    gr :: Graph SomeNode
    gr =
      f Graph.emptyGraph

{-----------------------------------------------------------------------------
    Set dependencies of individual notes
------------------------------------------------------------------------------}

-- | Add a child node to the children of a parent 'Pulse'.
connectChild ::
  -- | Parent node whose '_childP' field is to be updated.
  PulseRef a ->
  -- | Child node to add.
  SomeNode ->
  -- | Weak reference with the child as key and the parent as value.
  IO (Weak SomeNode)
connectChild parent child = do
  w <- mkWeakNodeValue child child
  modifyRef parent (update childrenP (w :))
  mkWeakNodeValue child (P parent) -- child keeps parent alive

-- | Add a child node to a parent node and update evaluation order.
doAddChild :: SomeNode -> SomeNode -> IO ()
doAddChild (P parent) (P child) = do
  level1 <- _levelP <$> readRef child
  level2 <- _levelP <$> readRef parent
  let level = level1 `max` (level2 + 1)
  w <- parent `connectChild` P child
  modifyRef child (set levelP level . update parentsP (w :))
doAddChild (P parent) node = void $ parent `connectChild` node
doAddChild x y = do
  sx <- printNode x
  sy <- printNode y
  error ("doAddChild (" ++ sx ++ ") (" ++ sy ++ ")")

-- | Remove a node from its parents and all parents from this node.
removeParents :: PulseRef a -> IO ()
removeParents child = do
  c@Pulse {_parentsP} <- readRef child
  -- delete this child (and dead children) from all parent nodes
  forM_ _parentsP $ \w -> do
    Just (P parent) <- deRefWeak w -- get parent node
    finalize w -- severe connection in garbage collector
    new <- filterM isGoodChild . _childrenP =<< readRef parent
    modifyRef parent (set childrenP new)
  -- replace parents by empty list
  writeRef child c {_parentsP = []}
  where
    isGoodChild w = not . maybe True (== P child) <$> deRefWeak w

-- | Set the parent of a pulse to a different pulse.
doChangeParent :: PulseRef a -> PulseRef b -> IO ()
doChangeParent child parent = do
  -- remove all previous parents and connect to new parent
  removeParents child
  w <- parent `connectChild` P child
  modifyRef child (update parentsP (w :))

  -- calculate level difference between parent and node
  levelParent <- _levelP <$> readRef parent
  levelChild <- _levelP <$> readRef child
  let d = levelParent - levelChild + 1
  -- level parent - d = level child - 1

  -- lower all parents of the node if the parent was higher than the node
  when (d > 0) $ do
    parents <- Graph.dfs (P parent) getParents
    for_ parents \(P node) -> do
      modifyRef node (update levelP (subtract d))

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
getParents :: SomeNode -> IO [SomeNode]
getParents (P p) = deRefWeaks . _parentsP =<< readRef p
getParents _ = return []
