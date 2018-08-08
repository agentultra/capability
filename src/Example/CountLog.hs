{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Example.CountLog where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader (ReaderT (..), runReaderT)
import Control.Monad.State.Strict (State, StateT, runState, runStateT)
import qualified Data.Char
import Data.Coerce (coerce)
import Data.IORef
import GHC.Generics (Generic)

import Has
import HasReader
import HasState


----------------------------------------------------------------------
-- Logger Capability


class Monad m => Logger m where
  logStr :: String -> m ()

-- | Any @HasReader "logger" (String -> IO ())@ can be a @Logger@.
newtype TheLoggerReader m a = TheLoggerReader (m a)
  deriving (Functor, Applicative, Monad)
instance
  (HasReader "logger" (String -> IO ()) m, MonadIO m)
  => Logger (TheLoggerReader m)
  where
    logStr msg =
      coerce (ask @"logger" >>= liftIO . ($ msg) :: m ())

-- Example program ---------------------------------------------------

-- | Log the given number.
logNum :: Logger m => Int -> m ()
logNum = logStr . ("num: " ++) . show

-- ReaderT instance --------------------------------------------------

data LogCtx = LogCtx { logger :: String -> IO () }
  deriving Generic
deriving via (TheField "logger" LogCtx)
  instance Has "logger" (String -> IO ()) LogCtx

regularLogger :: LogCtx
regularLogger = LogCtx { logger = putStrLn }

loudLogger :: LogCtx
loudLogger = LogCtx { logger = putStrLn . map Data.Char.toUpper }


newtype LogM m a = LogM (ReaderT LogCtx m a)
  deriving (Functor, Applicative, Monad)
  deriving Logger via (TheLoggerReader (ReaderT LogCtx m))

runLogM :: LogCtx -> LogM m a -> m a
runLogM ctx (LogM m) = runReaderT m ctx


----------------------------------------------------------------------
-- Counter Capability


class Monad m => Counter m where
  count :: m Int

-- | Any @HasState "counter" Int m@ can be a @Counter@.
newtype TheCounterState m a = TheCounterState (m a)
  deriving (Functor, Applicative, Monad)
instance
  (HasState "counter" Int m, Monad m)
  => Counter (TheCounterState m)
  where
    count = coerce @(m Int) $
      state @"counter" $ \n -> let !n' = n + 1 in (n', n')

-- Example program ---------------------------------------------------

-- | Use a counter to count up twice.
doubleCount :: Counter m => m Int
doubleCount = count >> count

-- StateT instance ---------------------------------------------------

-- XXX: Using just @StateT Int m a@ makes deriving via fail. Can we fix that?
newtype CounterM a = CounterM (State (TheValue Int) a)
  deriving (Functor, Applicative, Monad)
  deriving Counter via TheCounterState (State (TheValue Int))

runCounterM :: CounterM a -> (a, Int)
runCounterM (CounterM m) = runState m (TheValue 0) & _2 %~ theValue

-- ReaderT IORef instance --------------------------------------------

-- XXX: Why is @TheValue@ not required around @IORef Int@ here? See @CounterM@.
newtype Counter'M m a = Counter'M (ReaderT (IORef Int) m a)
  deriving (Functor, Applicative, Monad)
  deriving Counter
    via TheCounterState (TheReaderIORef (ReaderT (TheValue (IORef Int)) m))

runCounter'M :: MonadIO m => Counter'M m a -> m a
runCounter'M (Counter'M m) = runReaderT m =<< liftIO (newIORef 0)


----------------------------------------------------------------------
-- Mixed Capabilities

-- Example program ---------------------------------------------------

-- | Double count and log the result, repeat once.
mixed :: (Counter m, Logger m) => m ()
mixed = do
  doubleCount >>= logNum
  doubleCount >>= logNum

-- ReaderT instance --------------------------------------------------

data CountLogCtx = CountLogCtx
  { countCtx :: IORef Int
  , logCtx :: LogCtx
  } deriving Generic
deriving via (TheField "countCtx" CountLogCtx)
  instance Has "counter" (IORef Int) CountLogCtx
deriving via (TheFieldHas "logCtx" CountLogCtx)
  instance Has "logger" (String -> IO ()) CountLogCtx


newtype CountLogM m a = CountLogM (ReaderT CountLogCtx m a)
  deriving (Functor, Applicative, Monad)
  deriving Counter
    via (TheCounterState (TheReaderIORef (ReaderT CountLogCtx m)))
  deriving Logger
    via (TheLoggerReader (ReaderT CountLogCtx m))

runCountLogM :: MonadIO m => CountLogM m b -> m b
runCountLogM (CountLogM m) = do
  ref <- liftIO $ newIORef 0
  runReaderT m CountLogCtx
    { countCtx = ref
    , logCtx = regularLogger
    }
