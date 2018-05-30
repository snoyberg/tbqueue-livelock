{-# LANGUAGE RecordWildCards #-}
module UnliftedArrayBased (
        -- * TBQueue
        TBQueue,
        newTBQueueIO,
        readTBQueue,
        writeTBQueue,
        peekTBQueue,
  ) where

import GHC.Conc (STM, TVar, writeTVar, readTVar, retry, newTVarIO)
import qualified Data.Primitive.UnliftedArray as U
import Control.Monad (forM_)

-- | 'TBQueue' is an abstract type representing a bounded FIFO channel.
--
-- @since 2.4
data TBQueue a = TBQueue
  { nextRead :: !(TVar Int)
  , count :: !(TVar Int)
  , slots :: !(U.UnliftedArray (TVar a))
  }

-- |@IO@ version of 'newTBQueue'.  This is useful for creating top-level
-- 'TBQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTBQueueIO :: Int -> IO (TBQueue a)
newTBQueueIO size = TBQueue
  <$> newTVarIO 0
  <*> newTVarIO 0
  <*> replicateMU size (newTVarIO $ error "newTBQueueIO")

replicateMU :: U.PrimUnlifted a => Int -> IO a -> IO (U.UnliftedArray a)
replicateMU size action = do
  mu <- U.unsafeNewUnliftedArray size
  forM_ [0..size] $ \idx -> do
    x <- action
    U.writeUnliftedArray mu idx x
  U.unsafeFreezeUnliftedArray mu

-- |Write a value to a 'TBQueue'; blocks if the queue is full.
writeTBQueue :: TBQueue a -> a -> STM Bool
writeTBQueue TBQueue {..} a = do
      count' <- readTVar count
      if count' >= U.sizeofUnliftedArray slots
        then retry
        else do
          writeTVar count $ count' + 1
          read' <- readTVar nextRead
          let idx = (read' + count') `mod` U.sizeofUnliftedArray slots
          writeTVar (U.indexUnliftedArray slots idx) a
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
      writeTVar nextRead $! (read' + 1) `mod` U.sizeofUnliftedArray slots
      fmap Just $ readTVar $ U.indexUnliftedArray slots read'

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
peekTBQueue :: TBQueue a -> STM (Maybe a)
peekTBQueue TBQueue {..} = do
  count' <- readTVar count
  if count' == 0
    then retry
    else do
      read' <- readTVar nextRead
      fmap Just $ readTVar $ U.indexUnliftedArray slots read'
