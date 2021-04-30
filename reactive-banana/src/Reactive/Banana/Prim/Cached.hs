{-----------------------------------------------------------------------------
    reactive-banana
------------------------------------------------------------------------------}
{-# LANGUAGE RecursiveDo #-}

module Reactive.Banana.Prim.Cached
  ( -- | Utility for executing monadic actions once
    -- and then retrieving values from a cache.
    --
    -- Very useful for observable sharing.
    cache,
  )
where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- | An action whose result will be cached.
-- Executing the action the first time in the monad will
-- execute the side effects. From then on,
-- only the generated value will be returned.
{-# NOINLINE cache #-}
cache :: (MonadFix m, MonadIO m) => m a -> m a
cache action =
  unsafePerformIO do
    resultRef <- liftIO (newIORef Nothing)
    pure do
      liftIO (readIORef resultRef) >>= \case
        Nothing -> mdo
          liftIO (writeIORef resultRef (Just result))
          result <- action
          pure result
        Just result -> pure result
