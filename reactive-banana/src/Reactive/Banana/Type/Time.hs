module Reactive.Banana.Type.Time
  ( Time,
    agesAgo,
    beginning,
    next,
  )
where

-- | A timestamp local to this program run.
--
-- Useful e.g. for controlling cache validity.
newtype Time
  = T Integer
  deriving (Eq, Ord, Show, Read)

-- | Before the beginning of time. See Note [TimeStamp]
agesAgo :: Time
agesAgo =
  T (-1)

beginning :: Time
beginning =
  T 0

next :: Time -> Time
next (T n) =
  T (n + 1)

instance Semigroup Time where
  T x <> T y =
    T (max x y)

instance Monoid Time where
  mappend = (<>)
  mempty = beginning
