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

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.IORef
import Data.Monoid

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
  s <- get
  (a, s, w) <- liftIO $ f r s
  put s
  tell w
  return a

runRWSIO :: (Monoid w) => RWSIO r w s a -> (r -> s -> IO (a, s, w))
runRWSIO m r s = do
  w' <- newIORef mempty
  s' <- newIORef s
  a <- run m (Tuple r w' s')
  s <- readIORef s'
  w <- readIORef w'
  return (a, s, w)

tell :: (Monoid w) => w -> RWSIO r w s ()
tell w = R $ \(Tuple _ w' _) -> modifyIORef w' (`mappend` w)

ask :: RWSIO r w s r
ask = R $ \(Tuple r _ _) -> return r

get :: RWSIO r w s s
get = R $ \(Tuple _ _ s') -> readIORef s'

put :: s -> RWSIO r w s ()
put s = R $ \(Tuple _ _ s') -> writeIORef s' s
