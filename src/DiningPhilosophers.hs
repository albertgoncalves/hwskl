import Control.Concurrent (forkIO, myThreadId, threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, putTMVar, takeTMVar)
import Control.Monad (replicateM_)
import Text.Printf

philosopher :: TChan () -> TMVar () -> TMVar () -> IO ()
philosopher done left right = do
  let microseconds = 10000 :: Int
  threadName <- show <$> myThreadId

  printf "[ %s ] waiting for left fork\n" threadName
  atomically $ takeTMVar left
  threadDelay microseconds

  printf "[ %s ] waiting for right fork\n" threadName
  atomically $ takeTMVar right
  threadDelay microseconds

  printf "[ %s ] dining\n" threadName
  threadDelay microseconds

  printf "[ %s ] returning left fork\n" threadName
  atomically $ putTMVar left ()
  threadDelay microseconds

  printf "[ %s ] returning right fork\n" threadName
  atomically $ putTMVar right ()
  threadDelay microseconds

  printf "[ %s ] thinking\n" threadName
  atomically $ writeTChan done ()

main :: IO ()
main = do
  fork0 <- newTMVarIO ()
  fork1 <- newTMVarIO ()
  fork2 <- newTMVarIO ()
  fork3 <- newTMVarIO ()
  fork4 <- newTMVarIO ()

  done <- atomically newTChan

  mapM_
    (forkIO . uncurry (philosopher done))
    [ (fork0, fork1),
      (fork1, fork2),
      (fork2, fork3),
      (fork3, fork4),
      (fork0, fork4)
    ]

  replicateM_ 5 $ atomically $ readTChan done

  putStrLn "Done!"
