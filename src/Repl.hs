import Data.Functor ((<&>))
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

insert :: String -> Word -> String -> String
insert xs 0 y = y ++ xs
insert (x : xs) n y = x : insert xs (n - 1) y
insert [] _ y = y

flush :: IO ()
flush = hFlush stdout

printBuffer :: String -> IO ()
printBuffer x = putStr $ "=> " ++ x ++ "\n\n"

loop :: String -> IO ()
loop buffer = do
  putStr " ? "
  flush
  input <- getLine
  case input of
    "quit" -> return ()
    "new" -> do
      putStr " . "
      flush
      buffer' <- getLine
      printBuffer buffer'
      loop buffer'
    "show" -> do
      printBuffer buffer
      loop buffer
    "clear" -> do
      putStr "=>\n\n"
      loop ""
    "insert" -> do
      putStr " @ "
      flush
      i <- getLine
      maybe
        (loop buffer)
        ( \i' -> do
            putStr " . "
            flush
            buffer' <- getLine <&> insert buffer i'
            printBuffer buffer'
            loop buffer'
        )
        (readMaybe i)
    _ -> loop buffer

main :: IO ()
main = loop ""
