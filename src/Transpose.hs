import Data.List (elemIndex, intercalate)
import Data.Maybe (fromJust)

class ToInt a where
  toInt :: a -> Int

class FromChar a where
  fromChar :: Char -> a

class Transpose a where
  transpose :: Interval -> a -> a

data Letter = A | B | C | D | E | F | G

instance Show Letter where
  show A = "A"
  show B = "B"
  show C = "C"
  show D = "D"
  show E = "E"
  show F = "F"
  show G = "G"

instance ToInt Letter where
  toInt A = 9
  toInt B = 11
  toInt C = 0
  toInt D = 2
  toInt E = 4
  toInt F = 5
  toInt G = 7

instance FromChar Letter where
  fromChar 'A' = A
  fromChar 'B' = B
  fromChar 'C' = C
  fromChar 'D' = D
  fromChar 'E' = E
  fromChar 'F' = F
  fromChar 'G' = G
  fromChar _ = undefined

data Accidental = Flat | Sharp

instance Show Accidental where
  show Flat = "b"
  show Sharp = "#"

instance ToInt Accidental where
  toInt Flat = -1
  toInt Sharp = 1

instance FromChar Accidental where
  fromChar 'b' = Flat
  fromChar '#' = Sharp
  fromChar _ = undefined

data Note = Note Letter [Accidental]

instance Read Note where
  readsPrec _ (letter : accidentals) = [(Note (fromChar letter) $ map fromChar accidentals, [])]
  readsPrec _ _ = undefined

instance Show Note where
  show (Note letter accidentals) = show letter ++ intercalate "" (map show accidentals)

instance ToInt Note where
  toInt (Note letter accidentals) = (toInt letter + sum (map toInt accidentals)) `mod` 12

data Interval = Min2 | Maj2 | Min3 | Maj3 | Per4 | Aug4 | Dim5 | Per5 | Min6 | Maj6 | Min7 | Maj7

instance Read Interval where
  readsPrec _ "m2" = [(Min2, [])]
  readsPrec _ "M2" = [(Maj2, [])]
  readsPrec _ "m3" = [(Min3, [])]
  readsPrec _ "M3" = [(Maj3, [])]
  readsPrec _ "P4" = [(Per4, [])]
  readsPrec _ "A4" = [(Aug4, [])]
  readsPrec _ "d5" = [(Dim5, [])]
  readsPrec _ "P5" = [(Per5, [])]
  readsPrec _ "m6" = [(Min6, [])]
  readsPrec _ "M6" = [(Maj6, [])]
  readsPrec _ "m7" = [(Min7, [])]
  readsPrec _ "M7" = [(Maj7, [])]
  readsPrec _ _ = undefined

instance Show Interval where
  show Min2 = "m2"
  show Maj2 = "M2"
  show Min3 = "m3"
  show Maj3 = "M3"
  show Per4 = "P4"
  show Aug4 = "A4"
  show Dim5 = "d5"
  show Per5 = "P5"
  show Min6 = "m6"
  show Maj6 = "M6"
  show Min7 = "m7"
  show Maj7 = "M7"

instance ToInt Interval where
  toInt Min2 = 1
  toInt Maj2 = 2
  toInt Min3 = 3
  toInt Maj3 = 4
  toInt Per4 = 5
  toInt Aug4 = 6
  toInt Dim5 = 6
  toInt Per5 = 7
  toInt Min6 = 8
  toInt Maj6 = 9
  toInt Min7 = 10
  toInt Maj7 = 11

instance Transpose Letter where
  transpose interval letter =
    fromChar $
      letters !! ((fromJust (elemIndex (head $ show letter) letters) + steps interval) `mod` 7)
    where
      letters = "ABCDEFG"

      steps :: Interval -> Int
      steps Min2 = 1
      steps Maj2 = 1
      steps Min3 = 2
      steps Maj3 = 2
      steps Per4 = 3
      steps Aug4 = 3
      steps Dim5 = 4
      steps Per5 = 4
      steps Min6 = 5
      steps Maj6 = 5
      steps Min7 = 6
      steps Maj7 = 6

instance Transpose Note where
  transpose interval noteFrom@(Note letterFrom _) = Note letterTo accidentals
    where
      letterTo = transpose interval letterFrom
      distance = ((((toInt letterTo - toInt noteFrom) - toInt interval) + 6) `mod` 12) - 6
      accidentals
        | distance < 0 = replicate (-distance) Sharp
        | 0 < distance = replicate distance Flat
        | otherwise = []

parse :: [String] -> (Interval, [Note])
parse (interval : notes) = (read interval, map read notes)
parse _ = undefined

-- NOTE: $ shuf -n 1 -e m2 M2 m3 M3 P4 A4 d5 P5 m6 M6 m7 M7
main :: IO ()
main = interact $ show . (\(interval, notes) -> map (transpose interval) notes) . parse . words
