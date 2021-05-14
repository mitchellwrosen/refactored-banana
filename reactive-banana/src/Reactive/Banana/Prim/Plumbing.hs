module Reactive.Banana.Prim.Plumbing
  ( addChild,
    addOutput,
    alwaysP,
    askTime,
    buildLater,
    buildLaterReadNow,
    cachedLatch,
    changeParent,
    getValueL,
    keepAlive,
    liftBuildP,
    liftIOLater,
    neverP,
    newLatch,
    newPulse,
    pureL,
    readLatch,
    readPulseP,
    readPulseP',
    rememberLatchUpdate,
    rememberOutput,
    runBuildIO,
    runEvalP,
    unwrapEvalP,
    writePulseP,
  )
where

import Control.Monad (join)
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.RWSIO as RWS
import qualified Control.Monad.Trans.ReaderWriterIO as RW
import Data.Functor
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid (Endo (..))
import Data.Vault.Lazy (Vault)
import qualified Data.Vault.Lazy as Vault
import qualified Reactive.Banana.Prim.Dependencies as Deps
import Reactive.Banana.Prim.Types
import qualified Reactive.Banana.Type.Graph as Graph
import Reactive.Banana.Type.Ref
import Reactive.Banana.Type.Time (Time, agesAgo, beginning)
import System.IO.Unsafe

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}

-- | Make 'Pulse' from evaluation function
newPulse :: String -> EvalP (Maybe a) -> IO (Ref (Pulse a))
newPulse name eval = do
  key <- Vault.newKey
  newRef
    Pulse
      { _keyP = key,
        _seenP = agesAgo,
        _evalP = eval,
        _childrenP = [],
        _parentsP = [],
        _levelP = ground,
        _nameP = name
      }

{-
* Note [PulseCreation]

We assume that we do not have to calculate a pulse occurrence
at the moment we create the pulse. Otherwise, we would have
to recalculate the dependencies *while* doing evaluation;
this is a recipe for disaster.

-}

-- | 'Pulse' that never fires.
neverP :: IO (Ref (Pulse a))
neverP = do
  key <- Vault.newKey
  newRef
    Pulse
      { _keyP = key,
        _seenP = agesAgo,
        _evalP = pure Nothing,
        _childrenP = [],
        _parentsP = [],
        _levelP = ground,
        _nameP = "neverP"
      }

-- | Return a 'Latch' that has a constant value
pureL :: a -> Ref (Latch a)
pureL x =
  unsafePerformIO do
    newRef
      Latch
        { _seenL = beginning,
          _valueL = x,
          _evalL = pure x
        }

-- | Make new 'Latch' that can be updated by a 'Pulse'
newLatch :: forall a. a -> IO (Ref (Pulse a) -> Build (), Ref (Latch a))
newLatch x = mdo
  latchRef <-
    newRef
      Latch
        { _seenL = beginning,
          _valueL = x,
          _evalL = evalLatch latchRef
        }

  let updateOn :: Ref (Pulse a) -> Build ()
      updateOn pulseRef = do
        latchWriteRef <-
          liftIO do
            weakLatchRef <- mkWeakRef latchRef
            latchWriteRef <-
              newRef
                LatchWrite
                  { _evalLW = fromJust <$> readPulseP pulseRef,
                    _latchLW = weakLatchRef
                  }
            -- writer is alive only as long as the latch is alive
            latchRef `keepAlive` latchWriteRef
            pure latchWriteRef
        pulseRef `addChild` L latchWriteRef

  pure (updateOn, latchRef)
  where
    evalLatch :: Ref (Latch a) -> EvalL a
    evalLatch latchRef = do
      Latch {_seenL, _valueL} <- liftIO (readRef latchRef)
      RW.tell _seenL
      pure _valueL

-- | Make a new 'Latch' that caches a previous computation.
cachedLatch :: EvalL a -> Ref (Latch a)
cachedLatch eval =
  unsafePerformIO mdo
    latchRef <-
      newRef
        Latch
          { _seenL = agesAgo,
            _valueL = error "Undefined value of a cached latch.",
            _evalL = do
              latch@Latch {_seenL, _valueL} <- liftIO (readRef latchRef)
              -- calculate current value (lazy!) with timestamp
              (value, time) <- RW.listen eval
              liftIO $
                if time <= _seenL
                  then pure _valueL -- return old value
                  else do
                    -- update value
                    value `seq` writeRef latchRef latch {_seenL = time, _valueL = value}
                    pure value
          }
    pure latchRef

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Ref (Pulse EvalO) -> Build ()
addOutput p = do
  o <-
    liftIO $
      newRef
        Output
          { _evalO = fromMaybe (pure (pure ())) <$> readPulseP p
          }
  p `addChild` O o
  RW.tell mempty {newOutputs = [o]}

{-----------------------------------------------------------------------------
    Build monad
------------------------------------------------------------------------------}
runBuildIO :: BuildR -> Build a -> IO (a, IO (), [Ref Output])
runBuildIO i m0 = do
  (a, BuildW newEdges changedParents os liftIOLaters _) <- unfold mempty m0
  liftIOLaters -- execute late IOs
  pure (a, Deps.buildDependencies newEdges changedParents, os)
  where
    -- Recursively execute the  buildLater  calls.
    unfold :: BuildW -> Build a -> IO (a, BuildW)
    unfold w m = do
      (a, BuildW w1 w2 w3 w4 later) <- RW.runReaderWriterIO m i
      let w' = w <> BuildW w1 w2 w3 w4 mempty
      w'' <- case later of
        Just m1 -> snd <$> unfold w' m1
        Nothing -> pure w'
      pure (a, w'')

buildLater :: Build () -> Build ()
buildLater x =
  RW.tell mempty {lateBuild = Just x}

-- | Pretend to return a value right now,
-- but do not actually calculate it until later.
--
-- NOTE: Accessing the value before it's written leads to an error.
--
-- FIXME: Is there a way to have the value calculate on demand?
buildLaterReadNow :: Build a -> Build a
buildLaterReadNow m = do
  ref <-
    liftIO $
      newIORef $
        error "buildLaterReadNow: Trying to read before it is written."
  buildLater $ m >>= liftIO . writeIORef ref
  liftIO $ unsafeInterleaveIO $ readIORef ref

alwaysP :: Build (Ref (Pulse ()))
alwaysP = (\(_, x) -> x) <$> RW.ask

-- | @keepAlive x y@ establishes a relationship between @x@ and @y@ such that if @x@ is alive, @y@ is also
-- considered alive (even if all references to it are dropped).
keepAlive :: Ref a -> b -> IO ()
keepAlive x y =
  void (mkWeakRefValue x y)

addChild :: Ref (Pulse parent) -> Node -> Build ()
addChild parent child =
  RW.tell mempty {newEdges = Endo (Graph.insertEdge (P parent) child)}

-- | Assign a new parent to a child node.
-- INVARIANT: The child may have only one parent node.
changeParent :: Ref (Pulse child) -> Ref (Pulse parent) -> Build ()
changeParent child parent =
  RW.tell mempty {changedParents = [(P child, P parent)]}

liftIOLater :: IO () -> Build ()
liftIOLater x =
  RW.tell mempty {lateIO = x}

{-----------------------------------------------------------------------------
    EvalL monad
------------------------------------------------------------------------------}

-- | Evaluate a latch (-computation) at the latest time,
-- but discard timestamp information.
readLatch :: Ref (Latch a) -> IO a
readLatch latch = do
  Latch {_evalL} <- readRef latch
  fst <$> RW.runReaderWriterIO _evalL ()

getValueL :: Ref (Latch a) -> EvalL a
getValueL latch = do
  Latch {_evalL} <- liftIO (readRef latch)
  _evalL

{-----------------------------------------------------------------------------
    EvalP monad
------------------------------------------------------------------------------}
runEvalP :: Vault -> EvalP a -> Build (a, EvalPW)
runEvalP s1 m = RW.readerWriterIO $ \r2 -> do
  (a, _, (w1, w2)) <- RWS.runRWSIO m r2 s1
  return ((a, w1), w2)

liftBuildP :: Build a -> EvalP a
liftBuildP m = RWS.rws $ \r2 s -> do
  (a, w2) <- RW.runReaderWriterIO m r2
  return (a, s, (mempty, w2))

askTime :: EvalP Time
askTime = fst <$> RWS.ask

readPulseP :: Ref (Pulse a) -> EvalP (Maybe a)
readPulseP p = do
  Pulse {_keyP} <- liftIO (readRef p)
  vault <- RWS.get
  pure (join (Vault.lookup _keyP vault))

readPulseP' :: Vault -> Ref (Pulse a) -> IO (Maybe a)
readPulseP' vault pulseRef = do
  Pulse {_keyP} <- liftIO (readRef pulseRef)
  pure (join (Vault.lookup _keyP vault))

writePulseP :: Vault.Key (Maybe a) -> Maybe a -> EvalP ()
writePulseP key a = do
  s <- RWS.get
  RWS.put $ Vault.insert key a s

rememberLatchUpdate :: IO () -> EvalP ()
rememberLatchUpdate x = RWS.tell ((x, mempty), mempty)

rememberOutput :: (Ref Output, EvalO) -> EvalP ()
rememberOutput x = RWS.tell ((mempty, [x]), mempty)

-- worker wrapper to break sharing and support better inlining
unwrapEvalP :: RWS.Tuple r w s -> RWS.RWSIO r w s a -> IO a
unwrapEvalP r m = RWS.run m r
