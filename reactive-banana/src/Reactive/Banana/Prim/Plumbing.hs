{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Banana.Prim.Plumbing where

import Control.Monad (join)
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.RWSIO as RWS
import qualified Control.Monad.Trans.ReaderWriterIO as RW
import Data.Functor
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)
import Data.Vault.Lazy (Vault)
import qualified Data.Vault.Lazy as Vault
import qualified Reactive.Banana.Prim.Dependencies as Deps
import Reactive.Banana.Prim.Types
import Reactive.Banana.Prim.Util
import System.IO.Unsafe

{-----------------------------------------------------------------------------
    Build primitive pulses and latches
------------------------------------------------------------------------------}

-- | Make 'Pulse' from evaluation function
newPulse :: String -> EvalP (Maybe a) -> IO (Pulse a)
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
this is a recipe for desaster.

-}

-- | 'Pulse' that never fires.
neverP :: IO (Pulse a)
neverP = do
  key <- Vault.newKey
  newRef
    Pulse
      { _keyP = key,
        _seenP = agesAgo,
        _evalP = return Nothing,
        _childrenP = [],
        _parentsP = [],
        _levelP = ground,
        _nameP = "neverP"
      }

-- | Return a 'Latch' that has a constant value
pureL :: a -> Latch a
pureL a =
  unsafePerformIO do
    newRef
      Latch
        { _seenL = beginning,
          _valueL = a,
          _evalL = return a
        }

-- | Make new 'Latch' that can be updated by a 'Pulse'
newLatch :: forall a. a -> IO (Pulse a -> Build (), Latch a)
newLatch a = mdo
  latch <-
    newRef
      Latch
        { _seenL = beginning,
          _valueL = a,
          _evalL = do
            Latch {_seenL, _valueL} <- liftIO (readRef latch)
            RW.tell _seenL -- indicate timestamp
            return _valueL -- indicate value
        }
  let updateOn :: Pulse a -> Build ()
      updateOn p = do
        lw <-
          liftIO do
            w <- mkWeakRef latch
            lw <-
              newRef
                LatchWrite
                  { _evalLW = fromJust <$> readPulseP p,
                    _latchLW = w
                  }
            -- writer is alive only as long as the latch is alive
            latch `keepAlive` lw
            pure lw
        P p `addChild` L lw

  return (updateOn, latch)

-- | Make a new 'Latch' that caches a previous computation.
cachedLatch :: EvalL a -> Latch a
cachedLatch eval =
  unsafePerformIO mdo
    latch <-
      newRef
        Latch
          { _seenL = agesAgo,
            _valueL = error "Undefined value of a cached latch.",
            _evalL = do
              Latch {..} <- liftIO $ readRef latch
              -- calculate current value (lazy!) with timestamp
              (a, time) <- RW.listen eval
              liftIO $
                if time <= _seenL
                  then return _valueL -- return old value
                  else do
                    -- update value
                    let _seenL = time
                    let _valueL = a
                    a `seq` put latch (Latch {..})
                    return a
          }
    return latch

-- | Add a new output that depends on a 'Pulse'.
--
-- TODO: Return function to unregister the output again.
addOutput :: Pulse EvalO -> Build ()
addOutput p = do
  o <-
    liftIO $
      newRef
        Output
          { _evalO = fromMaybe (pure (pure ())) <$> readPulseP p
          }
  P p `addChild` O o
  RW.tell $ BuildW (mempty, [o], mempty, mempty)

{-----------------------------------------------------------------------------
    Build monad
------------------------------------------------------------------------------}
runBuildIO :: BuildR -> BuildIO a -> IO (a, IO (), [Output])
runBuildIO i m0 = do
  (a, BuildW (topologyUpdates, os, liftIOLaters, _)) <- unfold mempty m0
  liftIOLaters -- execute late IOs
  return (a, Deps.buildDependencies topologyUpdates, os)
  where
    -- Recursively execute the  buildLater  calls.
    unfold :: BuildW -> BuildIO a -> IO (a, BuildW)
    unfold w m = do
      (a, BuildW (w1, w2, w3, later)) <- RW.runReaderWriterIO m i
      let w' = w <> BuildW (w1, w2, w3, mempty)
      w'' <- case later of
        Just m1 -> snd <$> unfold w' m1
        Nothing -> return w'
      return (a, w'')

buildLater :: Build () -> Build ()
buildLater x = RW.tell $ BuildW (mempty, mempty, mempty, Just x)

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

getTimeB :: Build Time
getTimeB = (\(x, _) -> x) <$> RW.ask

alwaysP :: Build (Pulse ())
alwaysP = (\(_, x) -> x) <$> RW.ask

dependOn :: Pulse child -> Pulse parent -> Build ()
dependOn child parent =
  P parent `addChild` P child

-- | @keepAlive p1 p2@ establishes a relationship between @p1@ and @p2@ such that if @p1@ is alive, @p2@ is also
-- considered alive (even if all references to it are dropped).
keepAlive :: Ref child -> Ref parent -> IO ()
keepAlive child parent =
  void (mkWeakRefValue child parent)

addChild :: SomeNode -> SomeNode -> Build ()
addChild parent child =
  RW.tell $ BuildW (Deps.addChild parent child, mempty, mempty, mempty)

changeParent :: Pulse child -> Pulse parent -> Build ()
changeParent node parent =
  RW.tell $ BuildW (Deps.changeParent node parent, mempty, mempty, mempty)

liftIOLater :: IO () -> Build ()
liftIOLater x = RW.tell $ BuildW (mempty, mempty, x, mempty)

{-----------------------------------------------------------------------------
    EvalL monad
------------------------------------------------------------------------------}

-- | Evaluate a latch (-computation) at the latest time,
-- but discard timestamp information.
readLatch :: Latch a -> IO a
readLatch latch = do
  Latch {_evalL} <- readRef latch
  fst <$> RW.runReaderWriterIO _evalL ()

getValueL :: Latch a -> EvalL a
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

readPulseP :: Pulse a -> EvalP (Maybe a)
readPulseP p = do
  Pulse {_keyP} <- liftIO (readRef p)
  vault <- RWS.get
  pure (join (Vault.lookup _keyP vault))

writePulseP :: Vault.Key (Maybe a) -> Maybe a -> EvalP ()
writePulseP key a = do
  s <- RWS.get
  RWS.put $ Vault.insert key a s

rememberLatchUpdate :: IO () -> EvalP ()
rememberLatchUpdate x = RWS.tell ((x, mempty), mempty)

rememberOutput :: (Output, EvalO) -> EvalP ()
rememberOutput x = RWS.tell ((mempty, [x]), mempty)

-- worker wrapper to break sharing and support better inlining
unwrapEvalP :: RWS.Tuple r w s -> RWS.RWSIO r w s a -> IO a
unwrapEvalP r m = RWS.run m r
