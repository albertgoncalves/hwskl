import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import System.Timeout (timeout)

f :: Int -> Int -> IO (Maybe ())
f d t = do
  c <- newChan
  _ <- forkIO $ do
    threadDelay d
    writeChan c ()
  timeout t (readChan c)

main :: IO ()
main = mapM_ (print =<<) [f 500000 1000000, f 1000000 500000]
