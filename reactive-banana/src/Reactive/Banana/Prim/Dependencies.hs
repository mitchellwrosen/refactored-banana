{-# LANGUAGE RecordWildCards #-}

module Reactive.Banana.Prim.Dependencies
  ( -- | Utilities for operating on node dependencies.
    buildDependencies,
  )
where

import Data.Semigroup (Endo (..))
import Reactive.Banana.Prelude
import Reactive.Banana.Prim.Types (Node (P), Pulse (..), mkWeakNodeValue, printNode)
import Reactive.Banana.Type.Graph (Graph)
import qualified Reactive.Banana.Type.Graph as Graph
import Reactive.Banana.Type.Ref
import System.Mem.Weak

{-----------------------------------------------------------------------------
    Accumulate dependency information for nodes
------------------------------------------------------------------------------}

-- | Execute the information in the dependency builder to change network topology.
buildDependencies :: Endo (Graph Node) -> [(Node, Node)] -> IO ()
buildDependencies (Endo f) parents = do
  sequence_ [x `doAddChild` y | (x, y) <- Graph.getEdges gr]
  sequence_ [x `doChangeParent` y | (P x, P y) <- parents]
  where
    gr :: Graph Node
    gr =
      f Graph.emptyGraph

{-----------------------------------------------------------------------------
    Set dependencies of individual notes
------------------------------------------------------------------------------}

-- | Add a child node to the children of a parent 'Pulse'.
connectChild ::
  -- | Parent node whose '_childP' field is to be updated.
  Ref (Pulse a) ->
  -- | Child node to add.
  Node ->
  -- | Weak reference with the child as key and the parent as value.
  IO (Weak Node)
connectChild parent child = do
  w <- mkWeakNodeValue child child
  modifyRef parent \pulse@Pulse {_childrenP} -> pulse {_childrenP = w : _childrenP}
  mkWeakNodeValue child (P parent) -- child keeps parent alive

-- | Add a child node to a parent node and update evaluation order.
doAddChild :: Node -> Node -> IO ()
doAddChild (P parent) (P child) = do
  level1 <- _levelP <$> readRef child
  level2 <- _levelP <$> readRef parent
  let level = level1 `max` (level2 + 1)
  w <- parent `connectChild` P child
  modifyRef child \pulse@Pulse {_parentsP} ->
    pulse
      { _levelP = level,
        _parentsP = w : _parentsP
      }
doAddChild (P parent) node = void $ parent `connectChild` node
doAddChild x y = do
  sx <- printNode x
  sy <- printNode y
  error ("doAddChild (" ++ sx ++ ") (" ++ sy ++ ")")

-- | Remove a node from its parents and all parents from this node.
removeParents :: Ref (Pulse a) -> IO ()
removeParents child = do
  c@Pulse {_parentsP} <- readRef child
  -- delete this child (and dead children) from all parent nodes
  for_ _parentsP \w -> do
    Just (P parent) <- deRefWeak w -- get parent node
    finalize w -- severe connection in garbage collector
    new <- filterM isGoodChild . _childrenP =<< readRef parent
    modifyRef parent \pulse -> pulse {_childrenP = new}
  -- replace parents by empty list
  writeRef child c {_parentsP = []}
  where
    isGoodChild w = not . maybe True (== P child) <$> deRefWeak w

-- | Set the parent of a pulse to a different pulse.
doChangeParent :: Ref (Pulse a) -> Ref (Pulse b) -> IO ()
doChangeParent child parent = do
  -- remove all previous parents and connect to new parent
  removeParents child
  w <- parent `connectChild` P child
  modifyRef child \pulse@Pulse {_parentsP} -> pulse {_parentsP = w : _parentsP}

  -- calculate level difference between parent and node
  levelParent <- _levelP <$> readRef parent
  levelChild <- _levelP <$> readRef child
  let d = levelParent - levelChild + 1
  -- level parent - d = level child - 1

  -- lower all parents of the node if the parent was higher than the node
  when (d > 0) $ do
    parents <- Graph.dfs (P parent) getParents
    for_ parents \(P node) ->
      modifyRef node \pulse@Pulse {_levelP} -> pulse {_levelP = subtract d _levelP}

{-----------------------------------------------------------------------------
    Helper functions
------------------------------------------------------------------------------}
getParents :: Node -> IO [Node]
getParents (P p) = deRefWeaks . _parentsP =<< readRef p
getParents _ = return []
