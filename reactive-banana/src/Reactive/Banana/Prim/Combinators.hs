{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Banana.Prim.Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Debug.Trace
import Reactive.Banana.Prim.Plumbing
  ( cachedLatch,
    changeParent,
    dependOn,
    getValueL,
    keepAlive,
    liftBuildP,
    neverP,
    newLatch,
    newPulse,
    readLatch,
    readPulseP,
  )
import qualified Reactive.Banana.Prim.Plumbing (pureL)
import Reactive.Banana.Prim.Types (Build, EvalP, Future, Latch, Pulse)

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p1 = do
  p2 <- liftIO (newPulse "mapP" $ fmap f <$> readPulseP p1)
  p2 `dependOn` p1
  return p2

-- | Tag a 'Pulse' with future values of a 'Latch'.
--
-- This is in contrast to 'applyP' which applies the current value
-- of a 'Latch' to a pulse.
tagFuture :: Latch a -> Pulse b -> Build (Pulse (Future a))
tagFuture x p1 = do
  p2 <-
    liftIO do
      newPulse "tagFuture" $
        fmap (const (readLatch x)) <$> readPulseP p1
  p2 `dependOn` p1
  return p2

filterJustP :: Pulse (Maybe a) -> Build (Pulse a)
filterJustP p1 = do
  p2 <- liftIO (newPulse "filterJustP" $ join <$> readPulseP p1)
  p2 `dependOn` p1
  return p2

unsafeMapIOP :: forall a b. (a -> IO b) -> Pulse a -> Build (Pulse b)
unsafeMapIOP f p1 = do
  p2 <- liftIO (newPulse "unsafeMapIOP" $ eval =<< readPulseP p1)
  p2 `dependOn` p1
  return p2
  where
    eval :: Maybe a -> EvalP (Maybe b)
    eval (Just x) = Just <$> liftIO (f x)
    eval Nothing = return Nothing

unionWithP :: forall a. (a -> a -> a) -> Pulse a -> Pulse a -> Build (Pulse a)
unionWithP f px py = do
  p <- liftIO (newPulse "unionWithP" $ eval <$> readPulseP px <*> readPulseP py)
  p `dependOn` px
  p `dependOn` py
  return p
  where
    eval :: Maybe a -> Maybe a -> Maybe a
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing = Just x
    eval Nothing (Just y) = Just y
    eval Nothing Nothing = Nothing

-- See note [LatchRecursion]
applyP :: Latch (a -> b) -> Pulse a -> Build (Pulse b)
applyP f x = do
  p <- liftIO (newPulse "applyP" $ fmap <$> liftIO (readLatch f) <*> readPulseP x)
  p `dependOn` x
  return p

pureL :: a -> Latch a
pureL = Reactive.Banana.Prim.Plumbing.pureL

-- specialization of   mapL f = applyL (pureL f)
mapL :: (a -> b) -> Latch a -> Latch b
mapL f lx = cachedLatch $ f <$> getValueL lx

applyL :: Latch (a -> b) -> Latch a -> Latch b
applyL lf lx =
  cachedLatch $ getValueL lf <*> getValueL lx

accumL :: a -> Pulse (a -> a) -> Build (Latch a, Pulse a)
accumL a p1 = do
  (updateOn, x) <- liftIO (newLatch a)
  p2 <- applyP (mapL (\x f -> f x) x) p1
  updateOn p2
  return (x, p2)

-- specialization of accumL
stepperL :: a -> Pulse a -> Build (Latch a)
stepperL a p = do
  (updateOn, x) <- liftIO (newLatch a)
  updateOn p
  return x

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
switchL :: Latch a -> Pulse (Latch a) -> Build (Latch a)
switchL l pl = mdo
  x <- stepperL l pl
  return $ cachedLatch $ getValueL x >>= getValueL

executeP :: forall a b. Pulse (b -> Build a) -> b -> Build (Pulse a)
executeP p1 b = do
  p2 <- liftIO (newPulse "executeP" $ eval =<< readPulseP p1)
  p2 `dependOn` p1
  return p2
  where
    eval :: Maybe (b -> Build a) -> EvalP (Maybe a)
    eval =
      traverse \f -> liftBuildP (f b)

switchP :: forall a. Pulse (Pulse a) -> Build (Pulse a)
switchP pp = mdo
  never <- liftIO neverP
  lp <- stepperL never pp
  let -- switch to a new parent
      switch :: EvalP (Maybe ())
      switch =
        readPulseP pp >>= \case
          Nothing -> pure Nothing
          Just new -> do
            liftBuildP (p2 `changeParent` new)
            pure Nothing
      -- fetch value from old parent
      eval :: EvalP (Maybe a)
      eval = do
        pulse <- liftIO (readLatch lp)
        readPulseP pulse

  p1 <- liftIO (newPulse "switchP_in" switch :: IO (Pulse ()))
  p1 `dependOn` pp
  p2 <- liftIO (newPulse "switchP_out" eval)
  liftIO (p2 `keepAlive` p1)
  return p2
