import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import System.IO.Unsafe (unsafePerformIO)

counter :: IORef Int
{-# NOINLINE counter #-}
counter = unsafePerformIO $ newIORef 0

getCounter :: IO Int
getCounter = do
  c <- readIORef counter
  modifyIORef' counter succ
  return c

f :: (Show a) => a -> IO String
f x = (show x ++) . show <$> getCounter

loop :: IO a -> Int -> IO ()
loop _ 0 = return ()
loop f' n = f' >> loop f' (n - 1)

main :: IO ()
main = loop (putStrLn =<< f (1 :: Int)) 3
