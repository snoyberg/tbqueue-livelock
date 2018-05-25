import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Gauge
import qualified TBQueue as A

main :: IO ()
main = defaultMain
  [ bench "TBQueue" $ whnfIO $ run newTBQueueIO writeTBQueue readTBQueue
  , bench "vector based" $ whnfIO $ run A.newTBQueueIO A.writeTBQueue A.readTBQueue
  ]
  where
  run new write read = do
    q <- new 100
    concurrently_
      (replicateConcurrently_ 10 $ replicateM_ 100 $ atomically $ write q ())
      (replicateConcurrently_ 10 $ replicateM_ 100 $ atomically $ read q)
