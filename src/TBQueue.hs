module TBQueue (
        -- * TBQueue
        TBQueue,
        newTBQueueIO,
        readTBQueue,
        writeTBQueue,
        peekTBQueue,
  ) where

import GHC.Conc (STM, TVar, writeTVar, readTVar, retry, newTVarIO)

-- | 'TBQueue' is an abstract type representing a bounded FIFO channel.
--
-- @since 2.4
newtype TBQueue a = TBQueue (TVar (TBQueue' a))

data TBQueue' a = TBQueue'
  { cap :: !Int  -- number of things you can still write
  , relems :: [a]  -- R:  elements waiting to be read
  , welems :: ([a] -> [a])  -- W:  elements written
  }

-- |@IO@ version of 'newTBQueue'.  This is useful for creating top-level
-- 'TBQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTBQueueIO :: Int -> IO (TBQueue a)
newTBQueueIO size = TBQueue <$> newTVarIO TBQueue'
  { cap = size
  , relems = []
  , welems = id
  }

-- |Write a value to a 'TBQueue'; blocks if the queue is full.
writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue (TBQueue var) a = do
  t <- readTVar var
  if cap t == 0
    then retry
    else writeTVar var $! t
            { cap = cap t - 1
            , welems = welems t . (a:)
            }

-- |Read the next value from the 'TBQueue'.
readTBQueue :: TBQueue a -> STM a
readTBQueue (TBQueue var) = do
  t <- readTVar var
  case relems t of
    x:xs -> do
      writeTVar var $! t
        { cap = cap t + 1
        , relems = xs
        }
      return x
    [] ->
      case welems t [] of
        [] -> retry
        x:xs -> do
          writeTVar var $! TBQueue'
            { cap = cap t + 1
            , relems = xs
            , welems = id
            }
          pure x

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
peekTBQueue :: TBQueue a -> STM a
peekTBQueue (TBQueue var) = do
  t <- readTVar var
  case relems t of
    x:_ -> return x
    [] ->
      case welems t [] of
        [] -> retry
        xs@(x:_) -> do
          writeTVar var $! t
            { relems = xs
            , welems = id
            }
          pure x
