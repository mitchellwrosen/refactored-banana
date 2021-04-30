{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Reactive.Banana.Prim.Util where

import Control.Monad
import Control.Monad.IO.Class
import Data.Hashable
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Unique.Really
import qualified GHC.Base as GHC
import qualified GHC.IORef as GHC
import qualified GHC.STRef as GHC
import qualified GHC.Weak as GHC
import System.Mem.Weak

{-----------------------------------------------------------------------------
    IORefs that can be hashed
------------------------------------------------------------------------------}
data Ref a = Ref !(IORef a) !Unique

instance Hashable (Ref a) where hashWithSalt s (Ref _ u) = hashWithSalt s u

equalRef :: Ref a -> Ref b -> Bool
equalRef (Ref _ a) (Ref _ b) = a == b

newRef :: MonadIO m => a -> m (Ref a)
newRef a = liftIO $ liftM2 Ref (newIORef a) newUnique

readRef :: MonadIO m => Ref a -> m a
readRef ~(Ref ref _) = liftIO $ readIORef ref

put :: MonadIO m => Ref a -> a -> m ()
put ~(Ref ref _) = liftIO . writeIORef ref

-- | Strictly modify an 'IORef'.
modify' :: MonadIO m => Ref a -> (a -> a) -> m ()
modify' ~(Ref ref _) f = liftIO $ readIORef ref >>= \x -> writeIORef ref $! f x

{-----------------------------------------------------------------------------
    Weak pointers
------------------------------------------------------------------------------}
mkWeakIORefValueFinalizer :: IORef a -> value -> IO () -> IO (Weak value)
#if MIN_VERSION_base(4,9,0)
mkWeakIORefValueFinalizer r@(GHC.IORef (GHC.STRef r#)) v (GHC.IO f) = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
#else
mkWeakIORefValueFinalizer r@(GHC.IORef (GHC.STRef r#)) v f = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
#endif

mkWeakIORefValue :: IORef a -> value -> IO (Weak value)
mkWeakIORefValue a b = mkWeakIORefValueFinalizer a b (return ())

mkWeakRef :: Ref a -> IO (Weak (Ref a))
mkWeakRef ref@(Ref ref' _) =
  mkWeakIORefValueFinalizer ref' ref (pure ())

mkWeakRefValue :: MonadIO m => Ref a -> value -> m (Weak value)
mkWeakRefValue (Ref ref _) v = liftIO $ mkWeakIORefValue ref v

-- | Dereference a list of weak pointers while discarding dead ones.
deRefWeaks :: [Weak v] -> IO [v]
deRefWeaks ws = {-# SCC deRefWeaks #-} fmap catMaybes $ mapM deRefWeak ws
