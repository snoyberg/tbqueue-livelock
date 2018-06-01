{-# LANGUAGE RecordWildCards #-}
module VectorBased (
        -- * TBQueue
        TBQueue,
        newTBQueueIO,
        readTBQueue,
        writeTBQueue,
        peekTBQueue,
  ) where

import GHC.Conc (STM, TVar, writeTVar, readTVar, retry, newTVarIO)
import qualified Data.Vector as V
import Debug.Trace

data Pair = Pair
  { nextRead :: !Int
  , count :: !Int
  }

modAdd :: Int -- ^ size
       -> Int -- ^ x
       -> Int -- ^ y
       -> Int
modAdd size x y
    | out >= size = out - size
    | otherwise = out
  where
    out = x + y
{-# INLINE modAdd #-}

-- | 'TBQueue' is an abstract type representing a bounded FIFO channel.
--
-- @since 2.4
data TBQueue a = TBQueue
  { pair :: !(TVar Pair)
  , slots :: !(V.Vector (TVar a))
  }

-- |@IO@ version of 'newTBQueue'.  This is useful for creating top-level
-- 'TBQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTBQueueIO :: Int -> IO (TBQueue a)
newTBQueueIO size = TBQueue
  <$> newTVarIO (Pair 0 0)
  <*> V.replicateM size (newTVarIO $ error "newTBQueueIO")

-- |Write a value to a 'TBQueue'; blocks if the queue is full.
writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue TBQueue {..} a = do
      Pair read' count' <- readTVar pair
      if count' >= V.length slots
        then retry
        else do
          writeTVar pair $! Pair read' $ count' + 1
          let idx = modAdd (V.length slots) read' count'
          writeTVar (V.unsafeIndex slots idx) a
{-# INLINABLE writeTBQueue #-}

-- |Read the next value from the 'TBQueue'.
readTBQueue :: TBQueue a -> STM a
readTBQueue TBQueue {..} = do
  Pair read' count' <- readTVar pair
  if count' == 0
    then retry
    else do
      writeTVar pair $! Pair
        (modAdd (V.length slots) read' 1)
        (count' - 1)
      readTVar $ V.unsafeIndex slots read'
{-# INLINABLE readTBQueue #-}

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
peekTBQueue :: TBQueue a -> STM (Maybe a)
peekTBQueue TBQueue {..} = do
  Pair read' count' <- readTVar pair
  if count' == 0
    then retry
    else fmap Just $ readTVar $ V.unsafeIndex slots read'
{-# INLINABLE peekTBQueue #-}
