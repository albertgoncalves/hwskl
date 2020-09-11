import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Array.IArray (elems)
import Data.Array.MArray
  ( MArray,
    getBounds,
    newListArray,
    readArray,
    writeArray,
  )
import Data.Array.ST (STUArray, runSTUArray)
import Data.Foldable (for_)
import Data.Ix (Ix)

bubbleSort :: (MArray a e m, Ix i, Num i, Enum i, Ord e) => a i e -> m ()
bubbleSort xs = do
  (l, r) <- getBounds xs
  for_ [l .. r - 1] $ \i ->
    for_ [i + 1 .. r] $ \j -> do
      x <- readArray xs i
      y <- readArray xs j
      when (y < x) $ do
        writeArray xs i y
        writeArray xs j x

main :: IO ()
main = do
  print xs
  print xs'
  where
    xs = [8, 6, 4, 10, 1 :: Int]
    n = length xs - 1
    xs' = elems $
      runSTUArray $ do
        xs'' <- (newListArray (0, n) xs :: ST s (STUArray s Int Int))
        bubbleSort xs''
        return xs''
