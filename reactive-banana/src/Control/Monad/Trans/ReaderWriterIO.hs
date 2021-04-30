{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Trans.ReaderWriterIO
  ( -- * Synopsis

    -- | An implementation of the reader/writer monad transformer
    -- using an 'IORef' for the writer.

    -- * Documentation
    ReaderWriterIOT,
    readerWriterIOT,
    runReaderWriterIOT,
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
newtype ReaderWriterIOT r w a = ReaderWriterIOT {run :: r -> IORef w -> IO a}

instance Functor (ReaderWriterIOT r w) where
  fmap f m = ReaderWriterIOT $ \x y -> fmap f (run m x y)

instance Applicative (ReaderWriterIOT r w) where
  pure a = ReaderWriterIOT $ \_ _ -> pure a
  f <*> a = ReaderWriterIOT $ \x y -> run f x y <*> run a x y

instance Monad (ReaderWriterIOT r w) where
  return a = ReaderWriterIOT $ \_ _ -> return a
  m >>= k = ReaderWriterIOT $ \x y -> run m x y >>= \a -> run (k a) x y

instance MonadFix (ReaderWriterIOT r w) where
  mfix f = ReaderWriterIOT $ \x y -> mfix (\a -> run (f a) x y)

instance MonadIO (ReaderWriterIOT r w) where
  liftIO m = ReaderWriterIOT \_ _ -> m

instance (a ~ ()) => Semigroup (ReaderWriterIOT r w a) where
  mx <> my = mx >> my

instance (a ~ ()) => Monoid (ReaderWriterIOT r w a) where
  mempty = return ()
  mappend = (<>)

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
readerWriterIOT ::
  (Monoid w) =>
  (r -> IO (a, w)) ->
  ReaderWriterIOT r w a
readerWriterIOT f = do
  r <- ask
  (a, w) <- liftIO $ f r
  tell w
  return a

runReaderWriterIOT :: Monoid w => ReaderWriterIOT r w a -> r -> IO (a, w)
runReaderWriterIOT m r = do
  ref <- newIORef mempty
  a <- run m r ref
  w <- readIORef ref
  return (a, w)

tell :: (Monoid w) => w -> ReaderWriterIOT r w ()
tell w = ReaderWriterIOT $ \_ ref -> modifyIORef ref (`mappend` w)

listen :: (Monoid w) => ReaderWriterIOT r w a -> ReaderWriterIOT r w (a, w)
listen m = ReaderWriterIOT $ \r ref -> do
  a <- run m r ref
  w <- readIORef ref
  return (a, w)

local :: (r -> r) -> ReaderWriterIOT r w a -> ReaderWriterIOT r w a
local f m = ReaderWriterIOT $ \r ref -> run m (f r) ref

ask :: ReaderWriterIOT r w r
ask = ReaderWriterIOT $ \r _ -> return r
