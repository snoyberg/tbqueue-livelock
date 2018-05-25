{-# LANGUAGE RecordWildCards #-}
module TBQueue (
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

-- | 'TBQueue' is an abstract type representing a bounded FIFO channel.
--
-- @since 2.4
data TBQueue a = TBQueue
  { nextRead :: !(TVar Int)
  , count :: !(TVar Int)
  , slots :: !(V.Vector (TVar a))
  }

-- |@IO@ version of 'newTBQueue'.  This is useful for creating top-level
-- 'TBQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTBQueueIO :: Int -> IO (TBQueue a)
newTBQueueIO size = TBQueue
  <$> newTVarIO 0
  <*> newTVarIO 0
  <*> V.replicateM size (newTVarIO $ error "newTBQueueIO")

-- |Write a value to a 'TBQueue'; blocks if the queue is full.
writeTBQueue :: TBQueue a -> a -> STM Bool
writeTBQueue TBQueue {..} a = do
      count' <- readTVar count
      if count' >= V.length slots
        then retry
        else do
          writeTVar count $ count' + 1
          read' <- readTVar nextRead
          let idx = (read' + count') `mod` V.length slots
          writeTVar (V.unsafeIndex slots idx) a
          pure True

-- |Read the next value from the 'TBQueue'.
readTBQueue :: TBQueue a -> STM (Maybe a)
readTBQueue TBQueue {..} = do
  count' <- readTVar count
  if count' == 0
    then retry
    else do
      writeTVar count $! count' - 1
      read' <- readTVar nextRead
      writeTVar nextRead $! (read' + 1) `mod` V.length slots
      fmap Just $ readTVar $ V.unsafeIndex slots read'

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
peekTBQueue :: TBQueue a -> STM (Maybe a)
peekTBQueue TBQueue {..} = do
  count' <- readTVar count
  if count' == 0
    then retry
    else do
      read' <- readTVar nextRead
      fmap Just $ readTVar $ V.unsafeIndex slots read'
