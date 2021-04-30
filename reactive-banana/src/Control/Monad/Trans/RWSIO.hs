module Control.Monad.Trans.RWSIO
  ( -- * Synopsis

    -- | An implementation of the reader/writer/state monad transformer
    -- using an 'IORef'.

    -- * Documentation
    RWSIO (..),
    Tuple (..),
    rws,
    runRWSIO,
    tell,
    ask,
    get,
    put,
  )
where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.IORef

{-----------------------------------------------------------------------------
    Type and class instances
------------------------------------------------------------------------------}
data Tuple r w s = Tuple !r !(IORef w) !(IORef s)

newtype RWSIO r w s a = R {run :: Tuple r w s -> IO a}
  deriving stock (Functor)
  deriving (Applicative, Monad, MonadFix, MonadIO) via (ReaderT (Tuple r w s) IO)

{-----------------------------------------------------------------------------
    Functions
------------------------------------------------------------------------------}
rws :: (Monoid w) => (r -> s -> IO (a, s, w)) -> RWSIO r w s a
rws f = do
  r <- ask
  s0 <- get
  (a, s1, w) <- liftIO $ f r s0
  put s1
  tell w
  pure a

runRWSIO :: (Monoid w) => RWSIO r w s a -> (r -> s -> IO (a, s, w))
runRWSIO m r s = do
  w' <- newIORef mempty
  s' <- newIORef s
  a <- run m (Tuple r w' s')
  s1 <- readIORef s'
  w <- readIORef w'
  return (a, s1, w)

tell :: (Monoid w) => w -> RWSIO r w s ()
tell w = R $ \(Tuple _ w' _) -> modifyIORef w' (`mappend` w)

ask :: RWSIO r w s r
ask = R $ \(Tuple r _ _) -> return r

get :: RWSIO r w s s
get = R $ \(Tuple _ _ s') -> readIORef s'

put :: s -> RWSIO r w s ()
put s = R $ \(Tuple _ _ s') -> writeIORef s' s
