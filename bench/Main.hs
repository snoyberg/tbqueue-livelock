import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Gauge
import qualified VectorBased as V
import qualified OneTVar as One
import qualified AmTQueue1 as AmT
import qualified RTTQueue1 as RT1
import qualified RTTQueue2 as RT2

main :: IO ()
main = defaultMain
  [ bench "TChan" $ whnfIO $ run (const newTChanIO) writeTChan readTChan
  , bench "TQueue" $ whnfIO $ run (const newTQueueIO) writeTQueue readTQueue
  , bench "TBQueue" $ whnfIO $ run newTBQueueIO writeTBQueue readTBQueue
  , bench "vector based" $ whnfIO $ run V.newTBQueueIO V.writeTBQueue V.readTBQueue
  , bench "one tvar" $ whnfIO $ run One.newTBQueueIO One.writeTBQueue One.readTBQueue
  , bench "AmTQueue1" $ whnfIO $ run (const AmT.newTQueueIO) AmT.writeTQueue AmT.readTQueue
  , bench "RTTQueue1" $ whnfIO $ run (const RT1.newTQueueIO) RT1.writeTQueue RT1.readTQueue
  , bench "RTTQueue2" $ whnfIO $ run (const RT2.newTQueueIO) RT2.writeTQueue RT2.readTQueue
  ]
  where
  run new write read = do
    q <- new 100
    concurrently_
      (replicateConcurrently_ 10 $ replicateM_ 100 $ atomically $ write q ())
      (replicateConcurrently_ 10 $ replicateM_ 100 $ atomically $ read q)
