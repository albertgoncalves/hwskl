import Control.Monad (when)
import Data.Array.IO
  ( IOArray,
    getBounds,
    getElems,
    newListArray,
    readArray,
    writeArray,
  )
import Data.Foldable (for_)

bubbleSort :: Ord a => IOArray Int a -> IO ()
bubbleSort xs = do
  (l, r) <- getBounds xs
  for_ [l .. r - 1] $ \i ->
    for_ [i + 1 .. r] $ \j -> do
      x <- readArray xs i
      y <- readArray xs j
      when (y < x) $ do
        writeArray xs i y
        writeArray xs j x

fromList :: [a] -> IO (IOArray Int a)
fromList xs = newListArray (0, length xs - 1) xs

main :: IO ()
main = do
  xs <- fromList [4, 2, 3, 5, 1 :: Int]
  getElems xs >>= print
  bubbleSort xs
  getElems xs >>= print
