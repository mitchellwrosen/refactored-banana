{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Reactive.Banana.Prim.Combinators
  ( accumL,
    applyL,
    applyP,
    executeP,
    filterJustP,
    mapL,
    mapP,
    switchL,
    switchP,
    tagFuture,
    unionWithP,
    unsafeMapIOP,
  )
where

import Control.Monad
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.RWSIO as RWS
import Data.Function ((&))
import Reactive.Banana.Prim.Plumbing
  ( addChild,
    cachedLatch,
    changeParent,
    getValueL,
    keepAlive,
    liftBuildP,
    neverP,
    newLatch,
    newPulse,
    readLatch,
    readPulseP,
    readPulseP',
  )
import Reactive.Banana.Prim.Types (Build, EvalP, Future, Latch, Node (P), Pulse)
import Reactive.Banana.Type.Ref (Ref)

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
mapP :: forall a b. (a -> b) -> Ref (Pulse a) -> Build (Ref (Pulse b))
mapP f p1 = do
  p2 <- liftIO (newPulse "mapP" eval)
  p1 `addChild` P p2
  return p2
  where
    eval :: EvalP (Maybe b)
    eval = do
      vault <- RWS.get
      liftIO do
        value <- readPulseP' vault p1
        pure (f <$> value)

-- | Tag a 'Pulse' with future values of a 'Latch'.
--
-- This is in contrast to 'applyP' which applies the current value
-- of a 'Latch' to a pulse.
tagFuture :: forall a b. Ref (Latch a) -> Ref (Pulse b) -> Build (Ref (Pulse (Future a)))
tagFuture x p1 = do
  p2 <- liftIO (newPulse "tagFuture" eval)
  p1 `addChild` P p2
  return p2
  where
    eval :: EvalP (Maybe (Future a))
    eval = do
      vault <- RWS.get
      liftIO do
        value <- readPulseP' vault p1
        pure (readLatch x <$ value)

filterJustP :: Ref (Pulse (Maybe a)) -> Build (Ref (Pulse a))
filterJustP p1 = do
  p2 <- liftIO (newPulse "filterJustP" $ join <$> readPulseP p1)
  p1 `addChild` P p2
  return p2

unsafeMapIOP :: forall a b. (a -> IO b) -> Ref (Pulse a) -> Build (Ref (Pulse b))
unsafeMapIOP f p1 = do
  p2 <- liftIO (newPulse "unsafeMapIOP" $ eval =<< readPulseP p1)
  p1 `addChild` P p2
  pure p2
  where
    eval :: Maybe a -> EvalP (Maybe b)
    eval (Just x) = Just <$> liftIO (f x)
    eval Nothing = return Nothing

unionWithP :: forall a. (a -> a -> a) -> Ref (Pulse a) -> Ref (Pulse a) -> Build (Ref (Pulse a))
unionWithP f px py = do
  p <- liftIO (newPulse "unionWithP" $ eval <$> readPulseP px <*> readPulseP py)
  px `addChild` P p
  py `addChild` P p
  pure p
  where
    eval :: Maybe a -> Maybe a -> Maybe a
    eval (Just x) (Just y) = Just (f x y)
    eval (Just x) Nothing = Just x
    eval Nothing (Just y) = Just y
    eval Nothing Nothing = Nothing

-- See note [LatchRecursion]
applyP :: Ref (Latch (a -> b)) -> Ref (Pulse a) -> Build (Ref (Pulse b))
applyP f x = do
  p <- liftIO (newPulse "applyP" $ fmap <$> liftIO (readLatch f) <*> readPulseP x)
  x `addChild` P p
  return p

-- specialization of   mapL f = applyL (pureL f)
mapL :: (a -> b) -> Ref (Latch a) -> Ref (Latch b)
mapL f lx = cachedLatch $ f <$> getValueL lx

applyL :: Ref (Latch (a -> b)) -> Ref (Latch a) -> Ref (Latch b)
applyL lf lx =
  cachedLatch $ getValueL lf <*> getValueL lx

accumL :: a -> Ref (Pulse (a -> a)) -> Build (Ref (Latch a), Ref (Pulse a))
accumL a p1 = do
  (updateOn, latch) <- liftIO (newLatch a)
  p2 <- applyP (mapL (&) latch) p1
  updateOn p2
  return (latch, p2)

-- specialization of accumL
stepperL :: a -> Ref (Pulse a) -> Build (Ref (Latch a))
stepperL a p = do
  (updateOn, x) <- liftIO (newLatch a)
  updateOn p
  return x

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
switchL :: Ref (Latch a) -> Ref (Pulse (Ref (Latch a))) -> Build (Ref (Latch a))
switchL l pl = mdo
  x <- stepperL l pl
  pure (cachedLatch (getValueL x >>= getValueL))

executeP :: forall a b. Ref (Pulse (b -> Build a)) -> b -> Build (Ref (Pulse a))
executeP p1 b = do
  p2 <- liftIO (newPulse "executeP" $ eval =<< readPulseP p1)
  p1 `addChild` P p2
  pure p2
  where
    eval :: Maybe (b -> Build a) -> EvalP (Maybe a)
    eval =
      traverse \f -> liftBuildP (f b)

switchP :: forall a. Ref (Pulse (Ref (Pulse a))) -> Build (Ref (Pulse a))
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

  p1 <- liftIO (newPulse "switchP_in" switch :: IO (Ref (Pulse ())))
  pp `addChild` P p1
  p2 <- liftIO (newPulse "switchP_out" eval)
  liftIO (p2 `keepAlive` p1)
  return p2
