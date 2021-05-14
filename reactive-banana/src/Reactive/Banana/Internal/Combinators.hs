{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Reactive.Banana.Internal.Combinators where

import Control.Concurrent.MVar
import Control.Event.Handler
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Data.IORef
import Reactive.Banana.Prim (Build, Future, Latch)
import qualified Reactive.Banana.Prim as Prim
import Reactive.Banana.Prim.Types (Pulse)
import Reactive.Banana.Type.Ref (Ref)
import System.IO.Unsafe (unsafePerformIO)

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
type Behavior a = Moment (Ref (Latch a), Ref (Pulse ()))

type Event a = Moment (Ref (Pulse a))

type Moment = ReaderT EventNetwork Build

{-----------------------------------------------------------------------------
    IO
------------------------------------------------------------------------------}

-- | Data type representing an event network.
data EventNetwork = EventNetwork
  { runStep :: Prim.Step -> IO (),
    actuate :: IO (),
    pause :: IO ()
  }

-- | Compile to an event network.
compile :: Moment () -> IO EventNetwork
compile setup = do
  actuated <- newIORef False -- flag to set running status
  networkVar <- newEmptyMVar -- setup callback machinery
  let whenFlag flag action = readIORef flag >>= \b -> when b action
      runStep f = whenFlag actuated $ do
        s1 <- takeMVar networkVar -- read and take lock
        -- pollValues <- sequence polls     -- poll mutable data
        (output, s2) <- f s1 -- calculate new state
        putMVar networkVar s2 -- write state
        output -- run IO actions afterwards
      eventNetwork =
        EventNetwork
          { runStep = runStep,
            actuate = writeIORef actuated True,
            pause = writeIORef actuated False
          }

  -- compile initial graph
  network0 <- Prim.compile (runReaderT setup eventNetwork)
  putMVar networkVar network0 -- set initial state
  return eventNetwork

fromAddHandler :: AddHandler a -> Moment (Event a)
fromAddHandler addHandler = do
  (p, fire) <- lift Prim.newInput
  network <- ask
  _unregister <- liftIO (register addHandler (runStep network . fire))
  pure (pure p)

addReactimate :: Event (Future (IO ())) -> Moment ()
addReactimate e = do
  network <- ask
  lift $
    Prim.buildLater do
      -- Run cached computation later to allow more recursion with `Moment`
      p <- runReaderT e network
      Prim.addHandler p id

fromPoll :: IO a -> Moment (Behavior a)
fromPoll poll = do
  a <- liftIO poll
  e <- lift do
    p <- Prim.unsafeMapIOP (const poll) =<< Prim.alwaysP
    return $ pure p
  stepperB a e

imposeChanges :: Behavior a -> Event () -> Behavior a
imposeChanges behavior event =
  cache do
    (latch, _) <- behavior
    pulse <- event
    pure (latch, pulse)

{-----------------------------------------------------------------------------
    Combinators - basic
------------------------------------------------------------------------------}
never :: Event a
never =
  liftIO Prim.neverP

unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f event1 event2 =
  cache do
    pulse1 <- event1
    pulse2 <- event2
    lift (Prim.unionWithP f pulse1 pulse2)

filterJust :: Event (Maybe a) -> Event a
filterJust event =
  cache do
    pulse <- event
    lift (Prim.filterJustP pulse)

mapE :: (a -> b) -> Event a -> Event b
mapE f event =
  cache do
    pulse <- event
    lift (Prim.mapP f pulse)

applyE :: Behavior (a -> b) -> Event a -> Event b
applyE behavior event =
  cache do
    ~(latch, _) <- behavior
    pulse <- event
    lift (Prim.applyP latch pulse)

changesB :: Behavior a -> Event (Future a)
changesB behavior =
  cache do
    ~(latch, pulse) <- behavior
    lift (Prim.tagFuture latch pulse)

pureB :: a -> Behavior a
pureB x =
  cache do
    pulse <- never
    return (Prim.pureL x, pulse)

applyB :: Behavior (a -> b) -> Behavior a -> Behavior b
applyB behavior1 behavior2 =
  cache do
    ~(latch1, pulse1) <- behavior1
    ~(latch2, pulse2) <- behavior2
    lift do
      pulse3 <- Prim.unionWithP const pulse1 pulse2
      pure (Prim.applyL latch1 latch2, pulse3)

mapB :: (a -> b) -> Behavior a -> Behavior b
mapB f =
  applyB (pureB f)

{-----------------------------------------------------------------------------
    Combinators - accumulation
------------------------------------------------------------------------------}
-- Cache a computation at this moment in time
-- and make sure that it is performed in the Build monad eventually
cacheAndSchedule :: Moment a -> Moment (Moment a)
cacheAndSchedule m = do
  network <- ask
  lift do
    let c = cache (const m network) -- prevent let-floating!
    Prim.buildLater (void (runReaderT c network))
    return c

stepperB :: a -> Event a -> Moment (Behavior a)
stepperB x e =
  cacheAndSchedule do
    p0 <- e
    lift do
      p1 <- Prim.mapP const p0
      p2 <- Prim.mapP (const ()) p1
      (l, _) <- Prim.accumL x p1
      return (l, p2)

accumE :: a -> Event (a -> a) -> Moment (Event a)
accumE a event =
  cacheAndSchedule do
    pulse0 <- event
    lift do
      (_, pulse1) <- Prim.accumL a pulse0
      pure pulse1

{-----------------------------------------------------------------------------
    Combinators - dynamic event switching
------------------------------------------------------------------------------}
valueB :: Behavior a -> Moment a
valueB behavior = do
  ~(latch, _) <- behavior
  liftIO (Prim.readLatch latch)

initialBLater :: Behavior a -> Moment a
initialBLater behavior =
  mapReaderT Prim.buildLaterReadNow (valueB behavior)

executeP :: Ref (Pulse (Moment a)) -> Moment (Ref (Pulse a))
executeP pulse1 = do
  network <- ask
  lift do
    pulse2 <- Prim.mapP runReaderT pulse1
    Prim.executeP pulse2 network

observeE :: Event (Moment a) -> Event a
observeE event =
  cache do
    pulse <- event
    executeP pulse

executeE :: Event (Moment a) -> Moment (Event a)
executeE event = do
  -- Run cached computation later to allow more recursion with `Moment`
  pulse <- mapReaderT Prim.buildLaterReadNow (executeP =<< event)
  pure (pure pulse)

switchE :: Event (Event a) -> Moment (Event a)
switchE event = do
  network <- ask
  cacheAndSchedule do
    pulse1 <- event
    lift do
      pulse2 <- Prim.mapP runReaderT pulse1
      pulse3 <- Prim.executeP pulse2 network
      Prim.switchP pulse3

switchB :: Behavior a -> Event (Behavior a) -> Moment (Behavior a)
switchB b e = do
  network <- ask
  cacheAndSchedule do
    ~(l0, p0) <- b
    p1 <- e
    lift do
      p2 <- Prim.mapP runReaderT p1
      p3 <- Prim.executeP p2 network

      lr <- Prim.switchL l0 =<< Prim.mapP fst p3
      -- TODO: switch away the initial behavior
      let c1 = p0 -- initial behavior changes
      c2 <- Prim.mapP (const ()) p3 -- or switch happens
      c3 <- Prim.switchP =<< Prim.mapP snd p3 -- or current behavior changes
      pr <- merge c1 =<< merge c2 c3
      return (lr, pr)

merge :: Ref (Pulse ()) -> Ref (Pulse ()) -> Build (Ref (Pulse ()))
merge =
  Prim.unionWithP (\_ _ -> ())

-- | An action whose result will be cached.
-- Executing the action the first time in the monad will
-- execute the side effects. From then on,
-- only the generated value will be returned.
{-# NOINLINE cache #-}
cache :: Moment a -> Moment a
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
