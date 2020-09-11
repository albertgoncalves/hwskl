import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
  ( TQueue,
    isEmptyTQueue,
    newTQueue,
    readTQueue,
    writeTQueue,
  )
import Control.Monad (forever, when)
import System.Exit (exitSuccess)

main :: IO ()
main = do
  queue <- atomically newTQueue :: IO (TQueue Int)
  mapM_ (forkIO . atomically . writeTQueue queue) [1 .. 9]
  _ <- forkIO $ forever $ atomically (readTQueue queue) >>= print
  threadDelay 100
  forever $
    atomically (isEmptyTQueue queue) >>= \empty -> when empty exitSuccess
