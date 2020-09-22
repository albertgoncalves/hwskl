newtype Key = Key
  { unwrap :: Word
  }
  deriving (Show)

move :: Key -> [Key]
move (Key 0) = [Key 4, Key 6]
move (Key 1) = [Key 6, Key 8]
move (Key 2) = [Key 7, Key 9]
move (Key 3) = [Key 4, Key 8]
move (Key 4) = [Key 3, Key 9, Key 0]
move (Key 5) = []
move (Key 6) = [Key 1, Key 7, Key 0]
move (Key 7) = [Key 2, Key 6]
move (Key 8) = [Key 1, Key 3]
move (Key 9) = [Key 2, Key 4]
move _ = undefined

solve :: Word -> Key -> Word
solve 0 _ = 1
solve n k = sum $ map (solve $ n - 1) $ move k

main :: IO ()
main = print $ solve 16 $ Key 6
