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
  , slots :: !(U.UnliftedArray (TVar a))
  }

-- |@IO@ version of 'newTBQueue'.  This is useful for creating top-level
-- 'TBQueue's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newTBQueueIO :: Int -> IO (TBQueue a)
newTBQueueIO size = TBQueue
  <$> newTVarIO (Pair 0 0)
  <*> replicateMU size (newTVarIO $ error "newTBQueueIO")

replicateMU :: U.PrimUnlifted a => Int -> IO a -> IO (U.UnliftedArray a)
replicateMU size action = do
  mu <- U.unsafeNewUnliftedArray size
  forM_ [0..size] $ \idx -> do
    x <- action
    U.writeUnliftedArray mu idx x
  U.unsafeFreezeUnliftedArray mu

-- |Write a value to a 'TBQueue'; blocks if the queue is full.
writeTBQueue :: TBQueue a -> a -> STM ()
writeTBQueue TBQueue {..} a = do
      Pair read' count' <- readTVar pair
      if count' >= U.sizeofUnliftedArray slots
        then retry
        else do
          writeTVar pair $! Pair read' $ count' + 1
          let idx = modAdd (U.sizeofUnliftedArray slots) read' count'
          writeTVar (U.indexUnliftedArray slots idx) a
{-# INLINABLE writeTBQueue #-}

-- |Read the next value from the 'TBQueue'.
readTBQueue :: TBQueue a -> STM a
readTBQueue TBQueue {..} = do
  Pair read' count' <- readTVar pair
  if count' == 0
    then retry
    else do
      writeTVar pair $! Pair
        (modAdd (U.sizeofUnliftedArray slots) read' 1)
        (count' - 1)
      readTVar $ U.indexUnliftedArray slots read'
{-# INLINABLE readTBQueue #-}

-- | Get the next value from the @TBQueue@ without removing it,
-- retrying if the channel is empty.
peekTBQueue :: TBQueue a -> STM a
peekTBQueue TBQueue {..} = do
  Pair read' count' <- readTVar pair
  if count' == 0
    then retry
    else readTVar $ U.indexUnliftedArray slots read'
{-# INLINABLE peekTBQueue #-}
