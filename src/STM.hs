import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
  ( STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    retry,
    writeTVar,
  )
import Control.Monad (forever)
import System.Exit (exitSuccess)

loop :: Int -> IO a -> IO a
loop 1 m = m
loop n m = m >> loop (n - 1) m

transfer :: (Num a, Ord a) => a -> TVar a -> TVar a -> STM ()
transfer amount from to = do
  fromVal <- readTVar from
  if 0 <= (fromVal - amount)
    then do
      adjust (\x -> x - amount) from
      adjust (amount +) to
    else retry

adjust :: (a -> a) -> TVar a -> STM ()
adjust f account = do
  current <- readTVar account
  writeTVar account (f current)

getPair :: TVar a -> TVar a -> STM (a, a)
getPair a b = do
  a' <- readTVar a
  b' <- readTVar b
  return (a', b')

main :: IO ()
main = do
  a' <- newTVarIO a
  b' <- newTVarIO b
  _ <- loop n $ forkIO $ atomically $ transfer 1 a' b'
  forever $ do
    (a'', b'') <- atomically $ getPair a' b'
    putStrLn $ "  a : " ++ show a'' ++ "\n  b : " ++ show b''
    if a'' == m
      then exitSuccess
      else do
        putStrLn "Trying again!"
        threadDelay 1000
  where
    a = 10000 :: Int
    b = 4000 :: Int
    n = 2000 :: Int
    m = a - n
