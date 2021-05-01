module Reactive.Banana.Prim.Types
  ( Build,
    BuildR,
    BuildW (..),
    EvalL,
    EvalO,
    EvalP,
    EvalPW,
    Future,
    Latch' (..),
    Latch,
    LatchWrite (..),
    Level,
    Network (..),
    Node (..),
    Output (..),
    Pulse (..),
    Step,
    ground,
    mkWeakNodeValue,
    printNode,
  )
where

import Control.Monad.Trans.RWSIO
import Control.Monad.Trans.ReaderWriterIO
import Data.Hashable
import Data.Semigroup
import Data.Vault.Lazy (Vault)
import qualified Data.Vault.Lazy as Vault
import Reactive.Banana.Type.Graph (Graph)
import Reactive.Banana.Type.OSet (OSet)
import Reactive.Banana.Type.Ref
import Reactive.Banana.Type.Time (Time)
import System.IO.Unsafe
import System.Mem.Weak

{-----------------------------------------------------------------------------
    Network
------------------------------------------------------------------------------}

-- | A 'Network' represents the state of a pulse/latch network,
data Network = Network
  { -- | Current time.
    nTime :: !Time,
    -- | Remember outputs to prevent garbage collection.
    nOutputs :: !(OSet (Ref Output)),
    -- | Pulse that always fires.
    nAlwaysP :: !(Ref (Pulse ()))
  }

type EvalNetwork a = Network -> IO (a, Network)

type Step =
  EvalNetwork (IO ())

type Build =
  RWIO BuildR BuildW

-- ( current time
-- , pulse that always fires)
type BuildR = (Time, Ref (Pulse ()))

data BuildW = BuildW
  { -- | New edges to add to the network topology (all insertEdge calls)
    newEdges :: Endo (Graph Node),
    -- | Parent changes to make to the network topology
    changedParents :: [(Node, Node)],
    -- | Outputs to be added to the network
    newOutputs :: [Ref Output],
    -- | Late IO actions
    lateIO :: IO (),
    -- | Late build actions
    lateBuild :: Maybe (Build ())
  }

instance Semigroup BuildW where
  BuildW x1 x2 x3 x4 x5 <> BuildW y1 y2 y3 y4 y5 =
    BuildW (x1 <> y1) (x2 <> y2) (x3 <> y3) (x4 <> y4) (x5 <> y5)

instance Monoid BuildW where
  mappend = (<>)
  mempty = BuildW mempty mempty mempty mempty mempty

{-----------------------------------------------------------------------------
    Synonyms
------------------------------------------------------------------------------}

-- | Priority used to determine evaluation order for pulses.
type Level = Int

ground :: Level
ground =
  0

{-----------------------------------------------------------------------------
    Pulse and Latch
------------------------------------------------------------------------------}
data Pulse a = Pulse
  { _keyP :: Vault.Key (Maybe a), -- Key to retrieve pulse from cache.
    _seenP :: !Time, -- See note [Timestamp].
    _evalP :: EvalP (Maybe a), -- Calculate current value.
    _childrenP :: [Weak Node], -- Weak references to child nodes.
    _parentsP :: [Weak Node], -- Weak reference to parent nodes.
    _levelP :: !Level, -- Priority in evaluation order.
    _nameP :: String -- Name for debugging.
  }

instance Show (Ref (Pulse a)) where
  show p = _nameP (unsafePerformIO $ readRef p) ++ " " ++ show (hashWithSalt 0 p)

type Latch a = Ref (Latch' a)

data Latch' a = Latch
  { _seenL :: !Time, -- Timestamp for the current value.
    _valueL :: a, -- Current value.
    _evalL :: EvalL a -- Recalculate current latch value.
  }

data LatchWrite a = LatchWrite
  { _evalLW :: EvalP a, -- Calculate value to write.
    _latchLW :: Weak (Latch a) -- Destination 'Latch' to write to.
  }

newtype Output = Output
  { _evalO :: EvalP EvalO
  }

data Node where
  P :: Ref (Pulse a) -> Node
  L :: Ref (LatchWrite a) -> Node
  O :: Ref Output -> Node

instance Hashable Node where
  hashWithSalt s = \case
    P x -> hashWithSalt s x
    L x -> hashWithSalt s x
    O x -> hashWithSalt s x

instance Eq Node where
  P x == P y = equalRef x y
  L x == L y = equalRef x y
  O x == O y = equalRef x y
  x == y = error (unsafePerformIO (printNode x) ++ " /= " ++ unsafePerformIO (printNode y))

{-# INLINE mkWeakNodeValue #-}
mkWeakNodeValue :: Node -> v -> IO (Weak v)
mkWeakNodeValue = \case
  P x -> mkWeakRefValue x
  L x -> mkWeakRefValue x
  O x -> mkWeakRefValue x

-- | Evaluation monads.
type EvalPW = (IO (), [(Ref Output, EvalO)])

type EvalO = Future (IO ())

type Future = IO

-- Note: For efficiency reasons, we unroll the monad transformer stack.
-- type EvalP = RWST () Lazy.Vault EvalPW Build
type EvalP = RWSIO BuildR (EvalPW, BuildW) Vault

-- writer : (latch updates, IO action)
-- state  : current pulse values

-- Computation with a timestamp that indicates the last time it was performed.
type EvalL = RWIO () Time

{-----------------------------------------------------------------------------
    Show functions for debugging
------------------------------------------------------------------------------}
printNode :: Node -> IO String
printNode (P p) = _nameP <$> readRef p
printNode (L _) = return "L"
printNode (O _) = return "O"

{-----------------------------------------------------------------------------
    Notes
------------------------------------------------------------------------------}
{- Note [Timestamp]

The time stamp indicates how recent the current value is.

For Pulse:
During pulse evaluation, a time stamp equal to the current
time indicates that the pulse has already been evaluated in this phase.

For Latch:
The timestamp indicates the last time at which the latch has been written to.

    agesAgo   = The latch has never been written to.
    beginning = The latch has been written to before everything starts.

The second description is ensured by the fact that the network
writes timestamps that begin at time `next beginning`.

-}
