-- | Implementation of graph-related functionality
module Reactive.Banana.Type.Graph
  ( Graph,
    emptyGraph,
    getEdges,
    insertEdge,
    dfs,
  )
where

import Control.Monad.Trans.State.Strict
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashSet as HashSet
import Reactive.Banana.Prelude

-- | A directed graph.
--
-- Somewhat strange invariant: at least one node in this graph has no in-edges.
data Graph a = Graph
  { -- | Nodes related to the set of nodes connected by outgoing edges.
    -- Invariant: each list is non-empty, and does not contain dupes.
    children :: HashMap a [a],
    -- | Nodes related to the set of nodes connected by incoming edges.
    -- Invariant: each list is non-empty, and does not contain dupes.
    parents :: HashMap a [a],
    -- | All nodes.
    -- Invariant: equals (keys children `union` keys parents)
    nodes :: HashSet a
  }

-- | The graph with no edges and no nodes.
emptyGraph :: Graph a
emptyGraph =
  Graph HashMap.empty HashMap.empty HashSet.empty

-- | Insert an edge from the first node to the second node into the graph.
insertEdge :: (Eq a, Hashable a) => (a, a) -> Graph a -> Graph a
insertEdge (x, y) Graph {children, parents, nodes} =
  Graph
    { children = HashMap.insertWith (++) x [y] children,
      parents = HashMap.insertWith (++) y [x] parents,
      nodes = HashSet.insert x (HashSet.insert y nodes)
    }

getEdges :: (Eq a, Hashable a) => Graph a -> [(a, a)]
getEdges graph = do
  x <- toposort graph
  y <- getChildren graph x
  pure (x, y)

-- | Get all immediate children of a node in a graph.
getChildren :: (Eq a, Hashable a) => Graph a -> a -> [a]
getChildren Graph {children} x =
  HashMap.findWithDefault [] x children

-- | List all nodes such that each parent is listed before all of its children.
toposort :: forall a. (Eq a, Hashable a) => Graph a -> [a]
toposort gr =
  -- all nodes in topological order "parents before children"
  runIdentity (dfs' roots (Identity . getChildren gr))
  where
    -- all nodes without parents
    roots :: [a]
    roots =
      HashMap.keys (children gr `HashMap.difference` parents gr)

{-----------------------------------------------------------------------------
    Graph traversal
------------------------------------------------------------------------------}

-- | Graph represented as map of successors.
type GraphM m a =
  a -> m [a]

-- | Depth-first search. List all transitive successors of a node.
-- A node is listed *before* all its successors have been listed.
dfs :: (Eq a, Hashable a, Monad m) => a -> GraphM m a -> m [a]
dfs x =
  dfs' [x]

-- | Depth-first serach, refined version.
-- INVARIANT: None of the nodes in the initial list have a predecessor.
dfs' :: forall a m. (Eq a, Hashable a, Monad m) => [a] -> GraphM m a -> m [a]
dfs' xs0 succs =
  evalStateT (go [] xs0) HashSet.empty
  where
    go :: [a] -> [a] -> StateT (HashSet a) m [a]
    go acc = \case
      -- all nodes seen
      [] -> pure acc
      x : xs -> do
        seen <- get
        if HashSet.member x seen
          then go acc xs
          else do
            modify' (HashSet.insert x)
            xs' <- lift (succs x)
            -- visit all children
            acc' <- go acc xs'
            -- list this node as all successors have been seen
            go (x : acc') xs
