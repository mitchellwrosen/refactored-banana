{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.ReaderWriterIO
  ( -- * Synopsis

    -- | An implementation of the reader/writer monad transformer
    -- using an 'IORef' for the writer.

    -- * Documentation
    RWIO,
    readerWriterIO,
    runReaderWriterIO,
    tell,
    listen,
    ask,
    local,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.IORef
import Data.Monoid
import Data.Semigroup

{-----------------------------------------------------------------------------
    Type and class instances
------------------------------------------------------------------------------}
newtype RWIO r w a = RWIO {run :: r -> IORef w -> IO a}

instance Functor (RWIO r w) where
  fmap f m = RWIO $ \x y -> fmap f (run m x y)

instance Applicative (RWIO r w) where
  pure a = RWIO $ \_ _ -> pure a
  f <*> a = RWIO $ \x y -> run f x y <*> run a x y

instance Monad (RWIO r w) where
  return a = RWIO $ \_ _ -> return a
  m >>= k = RWIO $ \x y -> run m x y >>= \a -> run (k a) x y

instance MonadFix (RWIO r w) where
  mfix f = RWIO $ \x y -> mfix (\a -> run (f a) x y)

instance MonadIO (RWIO r w) where
  liftIO m = RWIO \_ _ -> m

instance (a ~ ()) => Semigroup (RWIO r w a) where
  mx <> my = mx >> my

instance (a ~ ()) => Monoid (RWIO r w a) where
  mempty = return ()
  mappend = (<>)

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
readerWriterIO ::
  (Monoid w) =>
  (r -> IO (a, w)) ->
  RWIO r w a
readerWriterIO f = do
  r <- ask
  (a, w) <- liftIO $ f r
  tell w
  return a

runReaderWriterIO :: Monoid w => RWIO r w a -> r -> IO (a, w)
runReaderWriterIO m r = do
  ref <- newIORef mempty
  a <- run m r ref
  w <- readIORef ref
  return (a, w)

tell :: (Monoid w) => w -> RWIO r w ()
tell w = RWIO $ \_ ref -> modifyIORef ref (`mappend` w)

listen :: (Monoid w) => RWIO r w a -> RWIO r w (a, w)
listen m = RWIO $ \r ref -> do
  a <- run m r ref
  w <- readIORef ref
  return (a, w)

local :: (r -> r) -> RWIO r w a -> RWIO r w a
local f m = RWIO $ \r ref -> run m (f r) ref

ask :: RWIO r w r
ask = RWIO $ \r _ -> return r
