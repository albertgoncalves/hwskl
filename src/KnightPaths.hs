import Data.Set (Set, empty, insert, member)

newtype Pos = Pos
  { getPos :: Word
  }
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

solve :: Set Pos -> [Pos] -> Word
solve s = sum . map f
  where
    f p = case filter (not . flip member s') $ move p of
      [] -> 1
      ps' -> solve s' ps'
      where
        s' = insert p s

main :: IO ()
main = print $ solve empty $ map Pos [0 .. 9]
