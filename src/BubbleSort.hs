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
  print items
  print sortedItems
  where
    items = [8.0, 6.0, 4.0, 10.0, 1.0 :: Float]
    sortedItems = elems $
      runSTUArray $ do
        xs <-
          newListArray
            (0, length items - 1)
            items ::
            ST s (STUArray s Int Float)
        bubbleSort xs
        return xs
