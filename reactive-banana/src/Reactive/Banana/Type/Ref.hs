{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module Reactive.Banana.Type.Ref
  ( Ref,
    newRef,
    equalRef,
    readRef,
    writeRef,
    modifyRef,
    mkWeakRef,
    mkWeakRefValue,
    deRefWeaks,
  )
where

import Data.Hashable (hashWithSalt)
import Data.IORef
import Data.Unique.Really
import qualified GHC.Base as GHC
import qualified GHC.IORef as GHC
import qualified GHC.STRef as GHC
import qualified GHC.Weak as GHC
import Reactive.Banana.Prelude
import System.Mem.Weak

-- | IORefs that can be hashed
data Ref a
  = Ref !(IORef a) !Unique

instance Eq (Ref a) where
  (==) =
    equalRef

instance Hashable (Ref a) where
  hashWithSalt s (Ref _ u) = hashWithSalt s u

equalRef :: Ref a -> Ref b -> Bool
equalRef (Ref _ a) (Ref _ b) =
  a == b

newRef :: a -> IO (Ref a)
newRef x =
  Ref <$> newIORef x <*> newUnique

readRef :: Ref a -> IO a
readRef ~(Ref ref _) =
  readIORef ref

writeRef :: Ref a -> a -> IO ()
writeRef ~(Ref ref _) =
  writeIORef ref

-- | Strictly modify a 'Ref'.
modifyRef :: Ref a -> (a -> a) -> IO ()
modifyRef ~(Ref ref _) =
  modifyIORef' ref

{-----------------------------------------------------------------------------
    Weak pointers
------------------------------------------------------------------------------}
mkWeakIORefValueFinalizer :: IORef a -> value -> IO () -> IO (Weak value)
mkWeakIORefValueFinalizer (GHC.IORef (GHC.STRef r#)) v (GHC.IO f) =
  GHC.IO \s ->
    case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)

mkWeakRef :: Ref a -> IO (Weak (Ref a))
mkWeakRef ref@(Ref ref' _) =
  mkWeakIORefValueFinalizer ref' ref (pure ())

mkWeakRefValue :: Ref a -> value -> IO (Weak value)
mkWeakRefValue (Ref ref _) value =
  mkWeakIORefValueFinalizer ref value (pure ())

-- | Dereference a list of weak pointers while discarding dead ones.
deRefWeaks :: [Weak v] -> IO [v]
deRefWeaks ws =
  catMaybes <$> traverse deRefWeak ws
