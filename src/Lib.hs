module Lib where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM (atomically)
import TBQueue
import Control.Monad
import Data.Functor
import System.Random
import System.TimeIt

transactionRepeats :: Int
transactionRepeats = 100

writeQ :: TBQueue Int -> IO ()
writeQ q = replicateM_ transactionRepeats $ do
  atomically $ void $ writeTBQueue q 0
  val <- randomRIO (1, 100)
  threadDelay val

readQ :: TBQueue Int -> IO ()
readQ q = replicateM_ transactionRepeats $ do
  atomically $ void $ readTBQueue q
  val <- randomRIO (1, 100)
  threadDelay val

peekQ :: TBQueue Int -> IO ()
peekQ q = void $ replicateM transactionRepeats $ do
  atomically $ void $ peekTBQueue q
  val <- randomRIO (1, 100)
  threadDelay val

asyncCount :: Int
asyncCount = 10

burn :: IO ()
burn = do
  tbq <- newTBQueueIO 10
  forever $ timeIt $ do
    (replicateConcurrently_ asyncCount (writeQ tbq) *> atomically (closeTBQueue tbq))
      `concurrently_` replicateConcurrently_ asyncCount (peekQ tbq)
      `concurrently_` replicateConcurrently_ asyncCount (readQ tbq)
    putStrLn "Successfully waited"
