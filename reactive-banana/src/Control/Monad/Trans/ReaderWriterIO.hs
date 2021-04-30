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

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.IORef

{-----------------------------------------------------------------------------
    Type and class instances
------------------------------------------------------------------------------}
newtype RWIO r w a = RWIO {run :: r -> IORef w -> IO a}
  deriving stock (Functor)
  deriving (Applicative, Monad, MonadFix, MonadIO) via (ReaderT r (ReaderT (IORef w) IO))

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
