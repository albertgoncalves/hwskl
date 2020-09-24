import Data.List (foldl')
import Data.Map.Strict (Map, empty, insert, lookup)
import Prelude hiding (lookup)

newtype Pos = Pos {getPos :: Word}
  deriving (Eq, Ord, Show)

newtype Step = Step {getStep :: Word}
  deriving (Eq, Ord, Show)

move :: Pos -> [Pos]
move (Pos 0) = [Pos 4, Pos 6]
move (Pos 1) = [Pos 6, Pos 8]
move (Pos 2) = [Pos 7, Pos 9]
move (Pos 3) = [Pos 4, Pos 8]
move (Pos 4) = [Pos 0, Pos 3, Pos 9]
move (Pos 6) = [Pos 0, Pos 1, Pos 7]
move (Pos 7) = [Pos 2, Pos 6]
move (Pos 8) = [Pos 1, Pos 3]
move (Pos 9) = [Pos 2, Pos 4]
move _ = []

type M = Map (Pos, Step) Word

solve :: (M, Word) -> (Pos, Step) -> (M, Word)
solve (m, v) k = case lookup k m of
  Just v' -> (m, v + v')
  Nothing -> (insert k v' m', v + v')
    where
      (p, s) = getStep <$> k
      (m', v')
        | s == 0 = (m, 1)
        | otherwise =
          foldl' solve (m, 0) $ zip (move p) (repeat $ Step $ s - 1)

main :: IO ()
main = print $ snd $ solve (empty, 0) (Pos 6, Step 32)
